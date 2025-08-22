module LCP where

import Types
import Parsers

import System.IO
import Data.List
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Search.AllValues
import System.Random

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
    mapM_ (\con -> do
        putStrLn $ show con
        seed <- getRandomSeed
        paths <- getOneValue $ dijkstraMulti adj con [] seed nrPaths
        case paths of
            Nothing -> return ()
            Just paths' -> mapM_ (writePath h con) paths'
      ) cons

type Path = ([Vertex], Float)

writePath :: Handle -> Connection -> Path -> IO ()
writePath h (Connection v1 v2) (vs,cost) =
    hPutStrLn h $ intercalate "," [show v1, show v2, show cost, showPath vs]
showPath :: [Vertex] -> String
showPath = intercalate ";" . map show

dijkstraMulti :: AdjacencyMap -> Connection -> [Path] -> Int -> Int -> [Path]
dijkstraMulti _ _ acc _ 0 = reverse acc
dijkstraMulti adj con acc seed nrPaths =
    let foundPath = dijkstra adj con
    in case foundPath of
        Nothing -> reverse acc
        Just p@(vertices,_) -> do
            --let newAdj = removeVertices adj (tail $ init vertices)
            -- remove random vertex
            --let randomVertexToRemove = head $ shuffle seed (tail $ init vertices)
            let randomVertexToRemove = getBiasedMiddleVertex seed vertices
                newAdj = removeVertices adj [randomVertexToRemove]
            dijkstraMulti newAdj con (p:acc) (head (nextInt seed)) (nrPaths-1)

getBiasedMiddleVertex :: Int -> [Vertex] -> Vertex
getBiasedMiddleVertex seed vs =
    let n         = length vs
        midIndex  = n `div` 2
        maxWeight = midIndex + 1
        -- create weighted list: middle gets maxWeight copies, edges get fewer
        weighted  = concat [ replicate (maxWeight - abs (i - midIndex)) v
                           | (v,i) <- zip vs [0..] ]
        shuffled  = shuffle seed weighted
    in head shuffled

dijkstra :: AdjacencyMap -> Connection -> Maybe Path
dijkstra adj (Connection start end) = go [(start,0,[start])] S.empty
  where
    go [] _ = Nothing
    go ((curPos,curCost,curPath):queue) visited
      | curPos == end = Just (reverse curPath, curCost)
      | curPos `S.member` visited = go queue visited
      | otherwise =
          let neighbors    = getNeighborsWithCost adj curPos
              updatedQueue = foldl update queue neighbors
          in go updatedQueue (S.insert curPos visited)
          where
              update :: [(Vertex,Float,[Vertex])] -> (Vertex,Float) -> [(Vertex,Float,[Vertex])]
              update accQueue (neighborVertex, edgeWeight)
                | neighborVertex `S.member` visited = accQueue
                | otherwise = insertBy (\(_,c1,_) (_,c2,_) -> c1 < c2)
                                       (neighborVertex, curCost+edgeWeight, neighborVertex:curPath)
                                       accQueue



