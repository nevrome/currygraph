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
    mapM_ (\con@(Connection v1 v2 _) -> do
        putStrLn $ show con
        let path = fromJust $ dijkstra adj v1 v2
        writePath h v1 v2 path
      ) cons

type Path = ([Vertex], Float)

writePath :: Handle -> Vertex -> Vertex -> Path -> IO ()
writePath h v1 v2 (vs,cost) =
    hPutStrLn h $ intercalate "," [show v1, show v2, show cost, showPath vs]
showPath :: [Vertex] -> String
showPath = intercalate ";" . map show

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



