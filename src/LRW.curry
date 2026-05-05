module LRW where

import Types
import Parsers

import System.IO
import Data.List
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Search.AllValues
import System.Random

data LRWOptions = LRWOptions {
      lrwVertFile :: String
    , lrwEdgeFile :: String
    , lrwFocalVertFile :: String
    , lrwNrPaths :: Int
    , lrwSeed :: Maybe Int
    , lrwOutFile :: String
}

runLRW :: LRWOptions -> IO ()
runLRW (
    LRWOptions
    vertFile edgeFile focalVertFile
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
    focalVertices <- readVertices focalVertFile
    putStrLn $ "Focal vertices: " ++ show (length focalVertices)
    putStrLn "Walking..."
    h <- openFile outFile WriteMode
    hPutStrLn h "v,num_path,sum_cost,path" -- csv header
    randomWalksForVertices h adj focalVertices nrPaths maybeSeed
    hFlush h
    hClose h
    putStrLn "Done"
    
randomWalksForVertices :: Handle
                          -> AdjacencyMap -> [Vertex]
                          -> Int -> (Maybe Int) -> IO ()
randomWalksForVertices h adj focalVs nrWalks maybeSeed = do
    seed <- case maybeSeed of
        Nothing -> getRandomSeed
        Just x -> return x
    let perFocalSeeds = take (length focalVs) $ nextInt seed
    mapM_ (\(v, perFocalSeed) -> do
        --putStrLn $ show v
        --hFlush stdout
        let walks = map (\w -> makeRandomWalk adj v (perFocalSeed + w) w) [1..nrWalks]
        mapM_ (writeWalk h v ) walks
      ) $ zip focalVs perFocalSeeds

type Walk = (Int, [Vertex], Float)

writeWalk :: Handle -> Vertex -> Walk -> IO ()
writeWalk h v (nrWalk, vs,cost) =
    hPutStrLn h $ intercalate "," [show v, show nrWalk, show cost, showVertexSequence vs]
showVertexSequence :: [Vertex] -> String
showVertexSequence = intercalate ";" . map show

makeRandomWalk :: AdjacencyMap -> Vertex -> Int -> Int -> Walk
makeRandomWalk adj focalV seed nrWalk = undefined
    



