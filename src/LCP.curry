module LCP where

import Types
import Parsers
import qualified PQueue as PQ

import System.IO
import Data.List
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (foldM_)
import Data.Functor.Const

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

pathsForConnections :: Handle -> AdjacencyMap -> [Connection] -> OmissionStrategy ->
                       S.Set Vertex -> CongestionStrategy -> IO ()
pathsForConnections h adj cons omissionStrategy dests congestionStrategy =
    case (omissionStrategy, congestionStrategy) of
        (OmitNone, IgnoreCongestion) -> pathsForConnectionsMany h adj cons
        _ -> pathsForConnectionsSequential h adj cons omissionStrategy dests congestionStrategy

pathsForConnectionsMany :: Handle -> AdjacencyMap -> [Connection] -> IO ()
pathsForConnectionsMany h adj cons = do
    let sourceGroups = M.toList groupedConnections
    allPaths <- searchSources (length sourceGroups) 1 M.empty sourceGroups
    mapM_ (writeConnection allPaths) cons
  where
    groupedConnections = foldl addConnection M.empty cons
    addConnection groups (Connection start end) =
        M.insertWith S.union start (S.singleton end) groups
    searchSources _ _ paths [] = return paths
    searchSources total current paths ((start, targets):remainingGroups) = do
        putStr $ "Searching from vertex " ++ show start ++ " (" ++ show current ++ "/" ++ show total ++ ", " ++ show (S.size targets) ++ " destinations)"
        hFlush stdout
        let sourcePaths = dijkstraMany adj start targets
        -- M.size forces the result sufficiently for the completion message to be printed
        putStrLn $ ": " ++ show (M.size sourcePaths) ++ " paths found"
        hFlush stdout
        let paths' = M.foldrWithKey (addPathForDestination start) paths sourcePaths
        searchSources total (current + 1) paths' remainingGroups
    addPathForDestination start end path paths =
        M.insert (start, end) path paths
    writeConnection paths con@(Connection start end) =
        case M.lookup (start, end) paths of
            Nothing   -> return ()
            Just path -> writePath h con path

dijkstraMany :: AdjacencyMap -> Vertex -> S.Set Vertex -> M.Map Vertex Path
dijkstraMany adj start targets = go (PQ.singleton (start, 0, [start], 0)) S.empty targets M.empty 1
  where
    -- queue entries: (current vertex, path cost, reversed path, insertion serial)
    go queue visited remaining paths nextSerial = next (PQ.minViewBy compareCost queue) visited remaining paths nextSerial
    next Nothing _ _ paths _ = paths
    next (Just ((curPos, curCost, curPath, _), queue)) visited remaining paths nextSerial
      | curPos `S.member` visited = go queue visited remaining paths nextSerial
      | otherwise =
          let visited' = S.insert curPos visited
              isRequested = curPos `S.member` remaining
              remaining' = if isRequested then S.delete curPos remaining else remaining
              paths' = if isRequested then M.insert curPos (Path (reverse curPath) curCost) paths else paths
          in if S.null remaining'
             then paths'
             else
                 let neighbors = getNeighborsWithCost adj curPos
                     (updatedQueue, updatedSerial) = foldl (update visited' curCost curPath) (queue, nextSerial) neighbors
                 in go updatedQueue visited' remaining' paths' updatedSerial
    update visited curCost curPath (accQueue, serial) (neighborVertex, edgeWeight)
      | neighborVertex `S.member` visited = (accQueue, serial)
      | otherwise = (PQ.insertBy compareCost newEntry accQueue, serial + 1)
          where
            newEntry =
                ( neighborVertex
                , curCost + edgeWeight
                , neighborVertex:curPath
                , serial
                )
    compareCost (_, cost1, _, serial1) (_, cost2, _, serial2) =
        cost1 < cost2 || (cost1 == cost2 && serial1 < serial2)

pathsForConnectionsSequential :: Handle -> AdjacencyMap -> [Connection] -> OmissionStrategy ->
                                 S.Set Vertex -> CongestionStrategy -> IO ()
pathsForConnectionsSequential h adj cons omissionStrategy dests congestionStrategy =
    foldM_ routeConnection M.empty cons
      where
        routeConnection :: EdgeUsage -> Connection -> IO EdgeUsage
        routeConnection usage con = do
            putStr $ "Searching for " ++ show con
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
        OmitNone  -> dijkstraCongested adj con omitNothing congestionStrategy usage
        OmitDests -> dijkstraCongested adj con isOmitted congestionStrategy usage
        Filter    -> dijkstraCongested adj con omitNothing congestionStrategy usage >>= rejectInHindsight
  where
    rejectInHindsight path@(Path vs _)
        | any isOmitted vs = Nothing
        | otherwise        = Just path
    isOmitted v = v /= start && v /= end && v `S.member` dests
    omitNothing _ = False

dijkstraCongested :: AdjacencyMap -> Connection -> (Vertex -> Bool) -> CongestionStrategy -> EdgeUsage -> Maybe Path
dijkstraCongested adj (Connection start end) isOmitted congestionStrategy usage =
    go (PQ.singleton (start, 0, 0, [start], 0)) S.empty 1
  where
    -- queue entries: (current vertex, routing cost including congestion, real path cost, path, insertion serial)
    go queue visited nextSerial = next (PQ.minViewBy compareRouteCost queue) visited nextSerial
    next Nothing _ _ = Nothing
    next (Just ((curPos, routeCost, realCost, curPath, _), queue)) visited nextSerial
      -- return the real graph cost, not the penalty-inflated cost
      | curPos == end = Just $ Path (reverse curPath) realCost
      | curPos `S.member` visited = go queue visited nextSerial
      | otherwise =
          let neighbors = getNeighborsWithCost adj curPos
              (updatedQueue, updatedSerial) = foldl update (queue, nextSerial) neighbors
          in go updatedQueue (S.insert curPos visited) updatedSerial
      where
        update (accQueue, serial) (neighborVertex, edgeWeight)
          | isOmitted neighborVertex = (accQueue, serial)
          | neighborVertex `S.member` visited = (accQueue, serial)
          | otherwise = (PQ.insertBy compareRouteCost newEntry accQueue, serial + 1)
          where
            newEntry =
                ( neighborVertex
                , routeCost + edgeWeight + congestionPenalty congestionStrategy usage curPos neighborVertex
                , realCost + edgeWeight
                , neighborVertex : curPath
                , serial
                )
    -- the serial preserves the input order and acts as a tie breaker
    -- it is not technically necessary, but it makes the output more predictable (at the cost of some performance)
    compareRouteCost (_, cost1, _, _, serial1) (_, cost2, _, _, serial2) =
        cost1 < cost2 || (cost1 == cost2 && serial1 < serial2)

congestionPenalty :: CongestionStrategy -> EdgeUsage -> Vertex -> Vertex -> Float
congestionPenalty IgnoreCongestion _ _ _ = 0
congestionPenalty (PenalizeUsedEdges penaltyPerUse) usage v1 v2 =
    let previousUses = M.findWithDefault 0 (edgeKey v1 v2) usage
    in penaltyPerUse * fromIntegral previousUses

writePath :: Handle -> Connection -> Path -> IO ()
writePath h (Connection v1 v2) (Path vs cost) =
    hPutStrLn h $ intercalate "," [show v1, show v2, show cost, showPath vs]
showPath :: [Vertex] -> String
showPath = intercalate ";" . map show

-- this is just kept here for reference, when I need a pure, simple dijkstra implementation
dijkstra :: AdjacencyMap -> Connection -> Maybe Path
dijkstra adj (Connection start end) = go (PQ.singleton (start, 0, [start])) S.empty
  where
    go queue visited = next (PQ.minViewBy compareCost queue) visited
    next Nothing _ = Nothing
    next (Just ((curPos, curCost, curPath), queue)) visited
      | curPos == end = Just $ Path (reverse curPath) curCost
      | curPos `S.member` visited = go queue visited
      | otherwise =
          let neighbors    = getNeighborsWithCost adj curPos
              updatedQueue = foldl update queue neighbors
          in go updatedQueue (S.insert curPos visited)
      where
        update accQueue (neighborVertex, edgeWeight)
          | neighborVertex `S.member` visited = accQueue
          | otherwise = PQ.insertBy compareCost
                                    (neighborVertex, curCost + edgeWeight, neighborVertex:curPath)
                                    accQueue
    compareCost (_, cost1, _) (_, cost2, _) = cost1 < cost2
