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
    


bfsLayersPruned :: AdjacencyMap -> S.Set Vertex -> Vertex -> [[Vertex]]
bfsLayersPruned adj destinationSet start = go S.empty 0 [start]
    where
        go _ _ [] = []
        go visited nrDestsFound layer
            | nrDestsFound >= 6 = []
            | otherwise =
                let (foundDestinations,expandable) = partition (\x -> S.member x destinationSetWithoutFocal) layer
                    newNrDestsFound = nrDestsFound + length foundDestinations
                    nowVisited = S.union visited (S.fromList layer)
                    nextLayer = nub $ concatMap (\v -> filter (isNotAlreadyVisited nowVisited) (getNeighbors adj v)) expandable
                in foundDestinations:(go nowVisited newNrDestsFound nextLayer)
        isNotAlreadyVisited :: S.Set Vertex -> Vertex -> Bool
        isNotAlreadyVisited visited v = not $ S.member v visited
        destinationSetWithoutFocal :: S.Set Vertex
        destinationSetWithoutFocal = S.delete start destinationSet
