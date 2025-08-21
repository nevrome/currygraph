module LCP where

import Types
import Parsers

import System.IO
import Data.List
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Set as S

data LCPOptions = LCPOptions {
      lcpVertFile :: String
    , lcpEdgeFile :: String
    , lcpConnectionFile :: String
    , lcpOutFile :: String
} deriving Show

runLCP :: LCPOptions -> IO ()
runLCP (
    LCPOptions
    vertFile edgeFile connectionFile
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
    putStrLn "Searching..."
    h <- openFile outFile WriteMode
    hPutStrLn h "v1,v2,initial_sum_cost,path" -- csv header
    pathsForConnections h adj connections
    hFlush h
    hClose h
    putStrLn "Done"

pathsForConnections :: Handle -> AdjacencyMap -> [Connection] -> IO ()
pathsForConnections h adj cons = do
    let nPaths = 3  -- how many shortest paths per connection
    --mapM_ (\(Connection v1 v2 _) ->  mapM_ (writePath h v1 v2) (yen adj v1 v2 nPaths)) cons
    mapM_ (\(Connection v1 v2 _) ->  mapM_ (writePath h v1 v2) [fromJust $ dijkstra adj v1 v2]) cons

writePath :: Handle -> Vertex -> Vertex -> Path -> IO ()
writePath h v1 v2 (vs,cost) =
    hPutStrLn h $ intercalate "," [show v1, show v2, show cost, showPath vs]
showPath :: [Vertex] -> String
showPath = intercalate ";" . map show

yen :: AdjacencyMap -> Vertex -> Vertex -> Int -> [Path]
yen adj start end maxPaths =
    case dijkstra adj start end of
        Nothing      -> []
        Just firstPath -> loop [firstPath] [] 1
  where
    loop pathResults _ count | count >= maxPaths = pathResults
    loop pathResults candidatePaths count =
      let prevPath       = pathResults !! (count-1)
          spurCandidates = concatMap (spurPaths prevPath pathResults) [0 .. length (fst prevPath) - 2]
          allCandidates  = nubBy (\(verts1,_) (verts2,_) -> verts1 == verts2) (candidatePaths ++ spurCandidates)
          sortedCands    = sortBy (\(_,cost1) (_,cost2) -> cost1 < cost2) allCandidates
      in case sortedCands of
           []              -> pathResults
           (bestPath:rest) -> loop (pathResults ++ [bestPath]) rest (count+1)

    spurPaths (pathVertices,_) existingResults spurIndex =
      let rootPathVertices = take (spurIndex+1) pathVertices
          spurNode         = last rootPathVertices
          removedEdges     = [ (path !! spurIndex, path !! (spurIndex+1))
                             | (path,_) <- existingResults
                             , take (spurIndex+1) path == rootPathVertices
                             ]
          prunedAdj        = foldl (\m (vFrom,vTo) -> remE vTo vFrom (remE vFrom vTo m))
                                   adj removedEdges
      in case dijkstra prunedAdj spurNode end of
           Nothing -> []
           Just (spurVertices, spurCost) ->
             let totalPathVertices = rootPathVertices ++ tail spurVertices
                 totalPathCost     = pathCost rootPathVertices + spurCost
             in [(totalPathVertices, totalPathCost)]

    remE vertex1 vertex2 = M.adjust (filter ((/= vertex2) . fst)) vertex1
    pathCost verts = sum [ cost
                         | (vFrom,vTo) <- zip verts (tail verts)
                         , Just cost   <- [lookup vTo (M.findWithDefault [] vFrom adj)] ]

dijkstra :: AdjacencyMap -> Vertex -> Vertex -> Maybe Path
dijkstra adj start end = go [(start,0,[start])] S.empty
  where
    go [] _ = Nothing
    go ((curPos,curCost,curPath):queue) visited
      | curPos == end = Just (reverse curPath, curCost)
      | curPos `S.member` visited = go queue visited
      | otherwise =
          let neighbors    = getNeighborsWithCost adj curPos
              updatedQueue = foldl updateQueueEntry queue neighbors
          in go updatedQueue (S.insert curPos visited)
          where
              updateQueueEntry :: [(Vertex,Float,[Vertex])] -> (Vertex,Float) -> [(Vertex,Float,[Vertex])]
              updateQueueEntry accQueue (neighborVertex, edgeWeight)
                | neighborVertex `S.member` visited = accQueue
                | otherwise = insertBy (\(_,c1,_) (_,c2,_) -> c1 < c2)
                                       (neighborVertex, curCost+edgeWeight, neighborVertex:curPath)
                                       accQueue





