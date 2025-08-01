module BFS where

import Types
import Parsers

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.List
import Control.Search.AllValues

data BFSOptions = BFSOptions {
      bfsVertFile :: String
    , bfsEdgeFile :: String
    , bfsOutFile :: String
} deriving Show

runBFS :: BFSOptions -> IO ()
runBFS (BFSOptions vertFile edgeFile outFile) = do
    putStrLn "Reading data..."
    vertices <- readVertices vertFile
    putStrLn $ "Vertices: " ++ show (length vertices)
    edges <- readEdges edgeFile vertices
    putStrLn $ "Edges: " ++ show (length edges)
    putStrLn "Building adjacency map..."
    let adj = buildAdjacencyMap edges
        verticesDest = S.fromList $ filter (\(Vertex _ _ _ f) -> f) vertices
        focal = fromJust $ find (\(Vertex v _ _ _) ->  v == 22) vertices
    putStrLn "Searching..."
    zuck <- getOneValue $ bfsLayersPruned adj verticesDest focal
    --putStrLn "Writing output..."
    --writePaths outFile connections paths
    putStrLn $ show $ zuck
    
type AdjacencyMap = M.Map Vertex [Vertex]

buildAdjacencyMap :: [Edge] -> AdjacencyMap
buildAdjacencyMap es = foldl addEdge M.empty es
  where
    addEdge m (Edge v1 v2 _) = M.insertWith (++) v1 [v2] $ M.insertWith (++) v2 [v1] m

bfsLayersPruned :: AdjacencyMap -> S.Set Vertex -> Vertex -> [[Vertex]]
bfsLayersPruned adj destinationSet start = bfs' S.empty 0 [start]
    where
        bfs' _ _ [] = []
        bfs' visited nrDestsFound layer
            | nrDestsFound >= 6 = []
            | otherwise =
                let foundDestinations = filter (\x -> S.member x destinationSetWithoutFocal) layer
                    nowVisited = S.union visited (S.fromList layer)
                    expandable = filter (\x -> not $ S.member x destinationSetWithoutFocal) layer
                    nextLayer = nub $ concatMap (\v -> filter (isNotVisited nowVisited) (getNeighbors adj v)) expandable
                    newNrDestsFound = nrDestsFound + length foundDestinations
                in foundDestinations:(bfs' nowVisited newNrDestsFound nextLayer)
        getNeighbors :: AdjacencyMap -> Vertex -> [Vertex]
        getNeighbors adj v = M.findWithDefault [] v adj
        isNotVisited :: S.Set Vertex -> Vertex -> Bool
        isNotVisited visited v = not $ S.member v visited
        destinationSetWithoutFocal :: S.Set Vertex
        destinationSetWithoutFocal = S.delete start destinationSet
