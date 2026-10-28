module LCP where

import Types
import Parsers
import qualified PQueue as PQ

import System.IO
import Data.List
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
dijkstraMany adj start targets = shortestPaths adj start targets (\_ _ edgeCost -> Just edgeCost)

pathsForConnectionsSequential :: Handle -> AdjacencyMap -> [Connection] -> OmissionStrategy ->
                                 S.Set Vertex -> CongestionStrategy -> IO ()
pathsForConnectionsSequential h adj cons omissionStrategy dests congestionStrategy =
    foldM_ routeConnection M.empty cons
      where
        routeConnection :: EdgeUsage -> Connection -> IO EdgeUsage
        routeConnection usage con = do
            putStrLn $ "Searching for " ++ show con
            hFlush stdout
            case pathForOneConnection adj omissionStrategy dests congestionStrategy usage con of
                Nothing -> pure usage
                Just path -> do
                    writePath h con path
                    pure $ addPath usage path

pathForOneConnection :: AdjacencyMap
                     -> OmissionStrategy -> S.Set Vertex -> CongestionStrategy -> EdgeUsage
                     -> Connection -> Maybe Path
pathForOneConnection adj omissionStrategy dests congestionStrategy usage con@(Connection start end) =
    case omissionStrategy of
        OmitNone  -> dijkstraCongested adj con (const False) congestionStrategy usage
        OmitDests -> dijkstraCongested adj con isOmitted congestionStrategy usage
        Filter    -> dijkstraCongested adj con (const False) congestionStrategy usage >>= rejectInHindsight
  where
    rejectInHindsight path@(Path vs _)
        | any isOmitted vs = Nothing
        | otherwise        = Just path
    isOmitted v = v /= start && v /= end && v `S.member` dests

dijkstraCongested :: AdjacencyMap -> Connection -> (Vertex -> Bool) -> CongestionStrategy
                     -> EdgeUsage -> Maybe Path
dijkstraCongested adj (Connection start end) isOmitted strategy usage =
    M.lookup end $ shortestPaths adj start (S.singleton end) routeIncrement
  where
    routeIncrement from to edgeCost
        | isOmitted to = Nothing
        | otherwise    = Just $ edgeCost + congestionPenalty strategy usage from to

congestionPenalty :: CongestionStrategy -> EdgeUsage -> Vertex -> Vertex -> Float
congestionPenalty IgnoreCongestion _ _ _ = 0
congestionPenalty (PenalizeUsedEdges penaltyPerUse) usage v1 v2 =
    let previousUses = M.findWithDefault 0 (edgeKey v1 v2) usage
    in penaltyPerUse * fromIntegral previousUses

-- actual path search
type Cost = Float
type RouteIncrement = Vertex -> Vertex -> Cost -> Maybe Cost
     -- vertex, routing cost, real cost, reversed path, insertion serial
data Candidate = Candidate Vertex Cost Cost [Vertex] Int

shortestPaths
    :: AdjacencyMap
    -> Vertex
    -> S.Set Vertex
    -> RouteIncrement
    -> M.Map Vertex Path
shortestPaths adj start targets routeIncrement =
    go initialQueue S.empty targets M.empty 1
  where
    initialQueue = PQ.singleton (Candidate start 0 0 [start] 0)
    go queue visited remaining paths nextSerial =
        case PQ.minViewBy compareCost queue of
            Nothing -> paths
            Just (candidate@(Candidate current _ realCost revPath _), queue')
                | current `S.member` visited -> go queue' visited remaining paths nextSerial
                | otherwise ->
                    let visited'= S.insert current visited
                        paths'  = if current `S.member` remaining
                                  then M.insert current (Path (reverse revPath) realCost) paths
                                  else paths
                        remaining' = S.delete current remaining
                    in if S.null remaining'
                       then paths'
                       else
                           let (queue'', nextSerial') =
                                   foldl
                                       (enqueue candidate visited')
                                       (queue', nextSerial)
                                       (getNeighborsWithCost adj current)
                            in go queue'' visited' remaining' paths' nextSerial'
    enqueue :: Candidate -> S.Set Vertex -> (PQ.PQueue Candidate, Int) -> (Vertex, Float) -> (PQ.PQueue Candidate, Int)
    enqueue
        (Candidate current routeCost realCost revPath _)
        visited
        (queue, serial)
        (neighbor, edgeCost)
        | neighbor `S.member` visited = (queue, serial)
        | otherwise =
            case routeIncrement current neighbor edgeCost of
                Nothing -> (queue, serial)
                Just extraCost ->
                    let c = Candidate neighbor (routeCost + extraCost) (realCost + edgeCost) (neighbor:revPath) serial
                    in (PQ.insertBy compareCost c queue, serial + 1)
    compareCost (Candidate _ cost1 _ _ serial1) (Candidate _ cost2 _ _ serial2) =
        cost1 < cost2 || (cost1 == cost2 && serial1 < serial2)

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
