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
    let colIDs = getCol "id" header rows
        maybeLongs = getColOptional "long" header rows
        maybeLats = getColOptional "lat" header rows
    case (maybeLongs, maybeLats) of
        (Just longs, Just lats) ->
            let spatposs = map (\(lo,la) -> makeSpatPos lo la) $ zip longs lats
            in return $ map (\(i,pos) -> makeVertex i (Just pos)) $ zip colIDs spatposs
        _ -> return $ map (\i -> makeVertex i Nothing) colIDs

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
    let connections = zipWith makeConnection verticesV1 verticesV2
    return connections

readPaths :: String -> VertexMap -> IO [Path]
readPaths path verticesMap = do
    header:rows <- readCSVFile path
    let colPaths = getCol "path" header rows
        pathVertices = map (\p -> map (findVertexUnsafe verticesMap . read) $ splitOn ";" p) colPaths
        colCosts = getCol "sum_cost" header rows
    let paths = zipWith makePath pathVertices colCosts
    return paths

getCol :: String -> [String] -> [[String]] -> [String]
getCol colName header rows =
    let colNum = fromJust $ getColNum colName header
    in map (\row -> row !! colNum) rows

getColOptional :: String -> [String] -> [[String]] -> Maybe [String]
getColOptional colName header rows =
    case getColNum colName header of
        Nothing -> Nothing
        Just colNum -> Just $ map (\row -> row !! colNum) rows

getColNum :: String -> [String] -> Maybe Int
getColNum colName header = findIndex (\x -> x == colName) header 
