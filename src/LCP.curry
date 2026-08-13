module LCP where

import Types
import Parsers

import System.IO
import Data.List
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (foldM_)

data LCPOptions = LCPOptions {
      lcpVertFile :: String
    , lcpEdgeFile :: String
    , lcpConnectionFile :: String
    , lcpOmissionStrategy :: OmissionStrategy
    , lcpDestFile :: Maybe String
    , lcpCongestionStrategy :: CongestionStrategy
    , lcpOutFile :: String
}

data OmissionStrategy = OmitNone | OmitDests | Filter

-- number of paths which have already used an edge
data CongestionStrategy = IgnoreCongestion | PenalizeUsedEdges Float

type EdgeUsage = M.Map (Vertex, Vertex) Int
addPath :: EdgeUsage -> Path -> EdgeUsage
addPath usage (Path vs _) = foldl addEdge usage (zip vs (drop 1 vs))
addEdge :: EdgeUsage -> (Vertex, Vertex) -> EdgeUsage
addEdge m (v1, v2) = M.insertWith (+) (edgeKey v1 v2) 1 m
-- edges are undirected in buildAdjacencyMap, so normalize their key
edgeKey :: Vertex -> Vertex -> (Vertex, Vertex)
edgeKey v1 v2
    | v1 <= v2  = (v1, v2)
    | otherwise = (v2, v1)


runLCP :: LCPOptions -> IO ()
runLCP (
    LCPOptions
    vertFile edgeFile connectionFile
    omissionStrategy maybeDestFile congestionStrategy
    outFile
    ) = do
    putStrLn "Reading data..."
    vertices <- readVertices vertFile
    let vm = buildVertexMap vertices
    putStrLn $ "Vertices: " ++ show (M.size vm)
    edges <- readEdges edgeFile vm
    putStrLn $ "Edges: " ++ show (length edges)
    putStrLn "Building adjacency map..."
    let adj = buildAdjacencyMap edges
    putStrLn $ "Size adjacency map: " ++ show (M.size adj) -- to force evaluation
    connections <- readConnections connectionFile vm
    putStrLn $ "Connections: " ++ show (length connections)
    dests <- case maybeDestFile of
        Nothing -> return S.empty
        Just destFile -> do
            verticesDest <- readVertices destFile
            let verticesDestSet = S.fromList verticesDest
            putStrLn $ "Destination vertices: " ++ show (S.size verticesDestSet)
            return verticesDestSet
    putStrLn "Searching..."
    h <- openFile outFile WriteMode
    hPutStrLn h "v1,v2,sum_cost,path" -- csv header
    pathsForConnections h adj connections omissionStrategy dests congestionStrategy
    hFlush h
    hClose h
    putStrLn "Done"

pathsForConnections :: Handle -> AdjacencyMap -> [Connection]
                       -> OmissionStrategy -> (S.Set Vertex) -> CongestionStrategy
                       -> IO ()
pathsForConnections h adj cons omissionStrategy dests congestionStrategy =
    foldM_ routeConnection M.empty cons
      where
        routeConnection :: EdgeUsage -> Connection -> IO EdgeUsage
        routeConnection usage con = do
            print con
            hFlush stdout
            case pathForConnection adj omissionStrategy dests congestionStrategy usage con of
                Nothing -> pure usage
                Just path -> do
                    writePath h con path
                    pure $ addPath usage path

pathForConnection :: AdjacencyMap
                     -> OmissionStrategy -> S.Set Vertex -> CongestionStrategy -> EdgeUsage
                     -> Connection -> Maybe Path
pathForConnection adj omissionStrategy dests congestionStrategy usage con@(Connection start end) =
    case omissionStrategy of
        OmitNone  -> dijkstraCongested adj con S.empty congestionStrategy usage
        OmitDests -> dijkstraCongested adj con toOmit congestionStrategy usage
        Filter    -> dijkstraCongested adj con S.empty congestionStrategy usage >>= rejectInHindsight
  where
    toOmit = dests `S.difference` S.fromList [start, end]
    rejectInHindsight path
        | shouldBeOmitted toOmit path = Nothing
        | otherwise                   = Just path
    shouldBeOmitted :: (S.Set Vertex) -> Path -> Bool
    shouldBeOmitted toOmit (Path vs _) = any (\v -> S.member v toOmit) vs

dijkstraCongested :: AdjacencyMap -> Connection -> S.Set Vertex -> CongestionStrategy -> EdgeUsage -> Maybe Path
dijkstraCongested adj (Connection start end) toOmit congestionStrategy usage =
    go [(start, 0, 0, [start])] S.empty
  where
    -- queue entries: (current vertex, routing cost including congestion, real path cost, path)
    go [] _ = Nothing
    go ((curPos, routeCost, realCost, curPath):queue) visited
        -- return the real graph cost, not the penalty-inflated cost
      | curPos == end = Just $ Path (reverse curPath) realCost
      | curPos `S.member` visited = go queue visited
      | otherwise =
          let neighbors = filter (\(v, _) -> not (v `S.member` toOmit)) $ getNeighborsWithCost adj curPos
              updatedQueue = foldl update queue neighbors
          in go updatedQueue (S.insert curPos visited)
      where
        update :: [(Vertex, Float, Float, [Vertex])] -> (Vertex, Float) -> [(Vertex, Float, Float, [Vertex])]
        update accQueue (neighborVertex, edgeWeight)
          | neighborVertex `S.member` visited = accQueue
          | otherwise = insertBy compareRouteCost newEntry accQueue
          where
            newEntry =
                ( neighborVertex
                , routeCost + edgeWeight + congestionPenalty congestionStrategy usage curPos neighborVertex
                , realCost + edgeWeight
                , neighborVertex : curPath
                )
        compareRouteCost (_, cost1, _, _) (_, cost2, _, _) = cost1 < cost2

congestionPenalty :: CongestionStrategy -> EdgeUsage -> Vertex -> Vertex -> Float
congestionPenalty IgnoreCongestion _ _ _ = 0
congestionPenalty (PenalizeUsedEdges penaltyPerUse) usage v1 v2 =
    let previousUses = M.findWithDefault 0 (edgeKey v1 v2) usage
    in penaltyPerUse * fromIntegral previousUses

dijkstra :: AdjacencyMap -> Connection -> S.Set Vertex -> Maybe Path
dijkstra adj (Connection start end) toOmit = go [(start,0,[start])] S.empty
  where
    go [] _ = Nothing
    go ((curPos,curCost,curPath):queue) visited
      | curPos == end = Just $ Path (reverse curPath) curCost
      | curPos `S.member` visited = go queue visited
      | otherwise =
          let neighbors     = getNeighborsWithCost adj curPos
              --updatedQueue = foldl update queue neighbors
              neighborsOmit = filter (\(v,_) -> not (S.member v toOmit)) neighbors
              updatedQueue = foldl update queue neighborsOmit
          in go updatedQueue (S.insert curPos visited)
          where
              update :: [(Vertex,Float,[Vertex])] -> (Vertex,Float) -> [(Vertex,Float,[Vertex])]
              update accQueue (neighborVertex, edgeWeight)
                | neighborVertex `S.member` visited = accQueue
                | otherwise = insertBy (\(_,c1,_) (_,c2,_) -> c1 < c2)
                                       (neighborVertex, curCost+edgeWeight, neighborVertex:curPath)
                                       accQueue

writePath :: Handle -> Connection -> Path -> IO ()
writePath h (Connection v1 v2) (Path vs cost) =
    hPutStrLn h $ intercalate "," [show v1, show v2, show cost, showPath vs]
showPath :: [Vertex] -> String
showPath = intercalate ";" . map show
