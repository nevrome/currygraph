module BFS where

import Types
import Parsers

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.List
import Control.Search.AllValues
import System.IO

data BFSOptions = BFSOptions {
      bfsVertFile :: String
    , bfsEdgeFile :: String
    , bfsDestFile :: String
    , bfsNrMinDests :: Int
    , bfsIncDestsByLayer :: Float
    , bfsStopAtDests :: Bool
    , bfsOutFile :: String
} deriving Show

runBFS :: BFSOptions -> IO ()
runBFS (
    BFSOptions
    vertFile edgeFile destFile
    nrMinDests incDestsByLayer
    stopAtDests
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
    verticesDest <- readVertices destFile
    let verticesDestSet = S.fromList verticesDest
    putStrLn $ "Destination vertices: " ++ show (S.size verticesDestSet)
    putStrLn "Searching..."
    h <- openFile outFile WriteMode
    hPutStrLn h "v1,v2,sum_cost" -- csv header
    mapM_ (bfsNClosest h adj verticesDestSet nrMinDests incDestsByLayer stopAtDests) verticesDest
    hFlush h
    hClose h
    putStrLn "Done"
    
bfsNClosest :: Handle -> AdjacencyMap -> S.Set Vertex -> Int -> Float -> Bool -> Vertex -> IO ()
bfsNClosest h adj destinationSet nrMinDests incDestsByLayer stopAtDests start = do
    destInLayers <- getOneValue $ bfsLayersPruned adj destinationSet nrMinDests incDestsByLayer stopAtDests start
    case destInLayers of
        Nothing -> return ()
        Just layers -> writeBFSResults h start layers

bfsLayersPruned :: AdjacencyMap -> S.Set Vertex -> Int -> Float -> Bool -> Vertex -> [[Vertex]]
bfsLayersPruned adj destinationSet nrMinDests incDestsByLayer stopAtDests start =
    go S.empty 0 (fromInt nrMinDests) [start]
    where
        go _ _ _ [] = []
        go visited nrDestsFound nrDestsToStop layer
            | nrDestsFound >= nrDestsToStop = []
            | otherwise =
                let (foundDestinations,expandable) = case stopAtDests of
                        True -> partition (\x -> S.member x destinationSetWithoutFocal) layer
                        False -> (filter (\x -> S.member x destinationSetWithoutFocal) layer, layer)
                    newNrDestsFound = nrDestsFound + fromInt (length foundDestinations)
                    nowVisited = S.union visited (S.fromList layer)
                    nextLayer = nub $ concatMap (\v -> filter (isNotAlreadyVisited nowVisited) (getNeighbors adj v)) expandable
                in foundDestinations:(go nowVisited newNrDestsFound (nrDestsToStop+incDestsByLayer) nextLayer)
        isNotAlreadyVisited :: S.Set Vertex -> Vertex -> Bool
        isNotAlreadyVisited visited v = not $ S.member v visited
        destinationSetWithoutFocal :: S.Set Vertex
        destinationSetWithoutFocal = S.delete start destinationSet

writeBFSResults :: Handle -> Vertex -> [[Vertex]] -> IO ()
writeBFSResults h start layers = do
    let indexedLayers = zip [1..] (drop 1 layers) -- skip layer 0: assumed to be always []
        triples = [(start, v, layerIdx) | (layerIdx, verts) <- indexedLayers, v <- verts]
    mapM_ (\(from, to, cost) -> hPutStrLn h (show from ++ "," ++ show to ++ "," ++ show cost)) triples
    
    
    
    
    
    
    