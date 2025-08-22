module LCP where

import Types
import Parsers

import System.IO
import Data.List
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Search.AllValues

data LCPOptions = LCPOptions {
      lcpVertFile :: String
    , lcpEdgeFile :: String
    , lcpConnectionFile :: String
    , lcpNrPaths :: Int
    , lcpOutFile :: String
} deriving Show

runLCP :: LCPOptions -> IO ()
runLCP (
    LCPOptions
    vertFile edgeFile connectionFile
    nrPaths
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
    pathsForConnections h adj connections nrPaths
    hFlush h
    hClose h
    putStrLn "Done"

pathsForConnections :: Handle -> AdjacencyMap -> [Connection] -> Int -> IO ()
pathsForConnections h adj cons nrPaths = do
    mapM_ (\(Connection v1 v2 _) -> do
        allPaths <- fromJust <$> getOneValue (yen adj v1 v2 nrPaths)
        mapM_ (writePath h v1 v2) allPaths
      ) cons

type Path = ([(Vertex, Float)], Float)
-- ([(vertex, cumulativeCostFromStart)], totalCost)

writePath :: Handle -> Vertex -> Vertex -> Path -> IO ()
writePath h v1 v2 (vs,cost) =
    hPutStrLn h $ intercalate "," [show v1, show v2, show cost, showPath vs]
showPath :: [(Vertex,Float)] -> String
showPath = intercalate ";" . map (show . fst)

yen :: AdjacencyMap -> Vertex -> Vertex -> Int -> [Path]
yen adj start end maxPaths =
    -- search first lcp
    case dijkstra adj start end of
        Nothing        -> []
        Just firstPath -> loop [firstPath] [] 1
    where
    -- find more paths
    loop :: [Path] -> [Path] -> Int -> [Path]
    loop pathResults _ count | count >= maxPaths = pathResults
    loop pathResults candidatePaths count =
        let prevPath    = pathResults !! (count-1)
            spurCands   = concatMap (spurPaths prevPath pathResults) [0 .. length (fst prevPath) - 2]
            allCands    = nubBy (\(vs1,_) (vs2,_) -> vs1 == vs2) (candidatePaths ++ spurCands)
            sortedCands = sortBy (\(_,cost1) (_,cost2) -> cost1 < cost2) allCands
        in case sortedCands of
             []              -> pathResults
             (bestPath:rest) -> loop (pathResults ++ [bestPath]) rest (count+1)
    spurPaths :: Path -> [Path] -> Int -> [Path]
    spurPaths (pathVertices,_) existingResults spurIndex =
      let rootPathVertices = take (spurIndex+1) pathVertices
          spurNode         = fst (last rootPathVertices)
          removedEdges     = [ (fst (path !! spurIndex), fst (path !! (spurIndex+1)))
                             | (path,_) <- existingResults
                             , take (spurIndex+1) path == rootPathVertices
                             ]
          prunedAdj        = removeEdges adj removedEdges
      in case dijkstra prunedAdj spurNode end of
           Nothing -> []
           Just (spurVertices, spurCost) ->
             let totalPathVertices = rootPathVertices ++ tail spurVertices
                 totalPathCost     = snd (last rootPathVertices) + spurCost
             in [(totalPathVertices, totalPathCost)]

dijkstra :: AdjacencyMap -> Vertex -> Vertex -> Maybe Path
dijkstra adj start end = go [(start,0,[(start,0)])] S.empty
  where
    go [] _ = Nothing
    go ((curPos,curCost,curPath):queue) visited
      | curPos == end        = Just (reverse curPath, curCost)
      | curPos `S.member` visited = go queue visited
      | otherwise =
          let neighbors = getNeighborsWithCost adj curPos
              updatedQueue = foldl updateQueueEntry queue neighbors
          in go updatedQueue (S.insert curPos visited)
        where
        updateQueueEntry :: [(Vertex,Float,[(Vertex,Float)])]
                         -> (Vertex,Float)
                         -> [(Vertex,Float,[(Vertex,Float)])]
        updateQueueEntry accQueue (neighborVertex, edgeWeight)
          | neighborVertex `S.member` visited = accQueue
          | otherwise =
              let newCost      = curCost + edgeWeight
                  newPathEntry = (neighborVertex, newCost) : curPath
              in insertBy (\(_,c1,_) (_,c2,_) -> c1 < c2)
                          (neighborVertex, newCost, newPathEntry)
                          accQueue



