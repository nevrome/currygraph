module Parsers where

import Types

import Data.Maybe
import Data.List
import Text.CSV (readCSVFile)
import qualified Data.Map as M

-- reading data
readVertices :: String -> IO [Vertex]
readVertices path = do
    header:rows <- readCSVFile path
    let colID = getCol "id" header rows
        colLong = getCol "long" header rows
        colLat = getCol "lat" header rows
    let vertices = zipWith3 makeVertex colID colLong colLat
    return vertices

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 _ []     _      _      _      = []
zipWith4 _ (_:_)  []     _      _      = []
zipWith4 _ (_:_)  (_:_)  []     _      = []
zipWith4 _ (_:_)  (_:_)  (_:_)  []     = []
zipWith4 f (x:xs) (y:ys) (z:zs) (w:ws) = f x y z w : zipWith4 f xs ys zs ws

readEdges :: String -> VertexMap -> IO [Edge]
readEdges path verticesMap = do
    header:rows <- readCSVFile path
    let colV1 = getCol "v1" header rows
        verticesV1 = map (findVertexUnsafe verticesMap . read) colV1
        colV2 = getCol "v2" header rows
        verticesV2 = map (findVertexUnsafe verticesMap . read) colV2
        colCost = getCol "cost" header rows
    let edges = zipWith3 makeEdge verticesV1 verticesV2 colCost
    return edges
    
readConnections :: String -> VertexMap -> IO [Connection]
readConnections path verticesMap = do
    header:rows <- readCSVFile path
    let colV1 = getCol "v1" header rows
        verticesV1 = map (findVertexUnsafe verticesMap . read) colV1
        colV2 = getCol "v2" header rows
        verticesV2 = map (findVertexUnsafe verticesMap . read) colV2
        colCost = getCol "sum_cost" header rows
    let connections = zipWith3 makeConnection verticesV1 verticesV2 colCost
    return connections

getCol :: String -> [String] -> [[String]] -> [String]
getCol colName header rows =
    let colNum = getColNum colName header
    in map (\row -> row !! colNum) rows
    where
        getColNum :: String -> [String] -> Int
        getColNum colName header = fromJust $ findIndex (\x -> x == colName) header 




