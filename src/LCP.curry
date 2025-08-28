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
    , lcpDestFile :: Maybe String
    , lcpNrPaths :: Int
    , lcpSeed :: Maybe Int
    , lcpOutFile :: String
} deriving Show

runLCP :: LCPOptions -> IO ()
runLCP (
    LCPOptions
    vertFile edgeFile connectionFile
    maybeDestFile
    nrPaths maybeSeed
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
    pathsForConnections h adj connections dests nrPaths maybeSeed
    hFlush h
    hClose h
    putStrLn "Done"

pathsForConnections :: Handle -> AdjacencyMap -> [Connection] -> (S.Set Vertex) -> Int -> (Maybe Int) -> IO ()
pathsForConnections h adj cons dests nrPaths maybeSeed = do
    seed <- case maybeSeed of
        Nothing -> getRandomSeed
        Just x -> return x
    let rands = take (length cons) $ nextInt seed
    mapM_ (\(con, rand) -> do
        putStrLn $ show con
        paths <- getOneValue $ dijkstraMulti adj con dests [] nrPaths rand
        case paths of
            Nothing -> return ()
            Just paths' -> mapM_ (writePath h con) paths'
      ) $ zip cons rands

type Path = ([Vertex], Float)

writePath :: Handle -> Connection -> Path -> IO ()
writePath h (Connection v1 v2) (vs,cost) =
    hPutStrLn h $ intercalate "," [show v1, show v2, show cost, showPath vs]
showPath :: [Vertex] -> String
showPath = intercalate ";" . map show

dijkstraMulti :: AdjacencyMap -> Connection -> (S.Set Vertex) -> [Path] -> Int -> Int -> [Path]
dijkstraMulti _ _ _ acc 0 _ = reverse acc
dijkstraMulti adj con@(Connection start end) dests acc nrPaths seed =
    -- optional mechanism to remove occupied nodes from the graph
    let toOmit = S.deleteAll [start, end] dests
        adjFiltered = removeVertices adj toOmit
    -- search paths
    in case dijkstra adjFiltered con of
        Nothing -> do
            dijkstraMulti adj con dests acc (nrPaths-1) (seed+1)
        Just p@(vertices,_) -> do
            let verticesWithoutStartEnd = tail $ init vertices
            -- remove all used vertices
            --let newAdj = removeVertices adj verticesWithoutStartEnd
            -- remove random vertex
            --let randomVertexToRemove = head $ shuffle seed verticesWithoutStartEnd
            -- remove weighted random vertex
            let randomVertexToRemove = getBiasedMiddleVertex seed verticesWithoutStartEnd
                newAdj = removeVertices adj (S.singleton randomVertexToRemove)
            dijkstraMulti newAdj con dests (p:acc) (nrPaths-1) (seed+1)

getBiasedMiddleVertex :: Int -> [Vertex] -> Vertex
getBiasedMiddleVertex seed vs =
    let n         = length vs
        midIndex  = n `div` 2
        maxWeight = midIndex + 1
        -- create weighted list: middle gets maxWeight copies, edges get fewer
        weighted  = concat [ replicate (maxWeight - abs (i - midIndex)) v | (v,i) <- zip vs [0..] ]
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



