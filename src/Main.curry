module Main where

import Data.List
import Data.Maybe
import qualified OptParse as OP
import System.Environment
import Control.Search.AllValues
import Text.CSV (readCSVFile, writeCSVFile)

data Options = Options {
      edgeFile :: String
    , connectionFile :: String
    , outFile :: String
} deriving Show

cmdParser = OP.optParser $
    OP.option (\s a -> a { edgeFile = s }) (
        OP.long "edgeFile"
        OP.<> OP.short "e"
        OP.<> OP.metavar "PATH"
        OP.<> OP.help "..."
    ) OP.<.> OP.option (\s a -> a { connectionFile = s }) (
        OP.long "connectionFile"
        OP.<> OP.short "c"
        OP.<> OP.metavar "PATH"
        OP.<> OP.help "..."
    ) OP.<.> OP.option (\s a -> a { outFile = s }) (
        OP.long "outFile"
        OP.<> OP.short "o"
        OP.<> OP.metavar "PATH"
        OP.<> OP.help "..."
    )

applyParse :: [Options -> Options] -> Options
applyParse fs = foldl (flip apply) defaultOpts fs
    where
        defaultOpts = Options "" "" ""

main :: IO ()
main = do
  args <- getArgs
  parseResult <- return $ OP.parse (intercalate " " args) cmdParser "cnn"
  case parseResult of
    Left err -> putStrLn err
    Right  v -> do
        let options = applyParse v
        runCNN options

-- reading data
readEdges :: String -> IO [Edge]
readEdges path = do
    header:rows <- readCSVFile path
    let colV1 = getCol "v1" header rows
        colV2 = getCol "v2" header rows
        colCost = getCol "cost" header rows
    let edges = zipWith3 makeEdge colV1 colV2 colCost
    return edges
readConnections :: String -> IO [Connection]
readConnections path = do
    header:rows <- readCSVFile path
    let colV1 = getCol "v1" header rows
        colV2 = getCol "v2" header rows
    let connections = zipWith makeConnection colV1 colV2
    return connections
getCol :: String -> [String] -> [[String]] -> [String]
getCol colName header rows =
    let colNum = getColNum colName header
    in map (\row -> row !! colNum) rows
    where
        getColNum :: String -> [String] -> Int
        getColNum colName header = fromJust $ findIndex (\x -> x == colName) header 

runCNN :: Options -> IO ()
runCNN (Options edgeFile connectionFile outFile) = do
    -- prepare data
    edges <- readEdges edgeFile
    connections <- readConnections connectionFile
    -- search
    paths <- pathForConnections edges connections
    -- write results
    let outCSV = prepOutCSV connections paths
    writeCSVFile outFile outCSV
    putStrLn "Done"

prepOutCSV :: [Connection] -> [Maybe [Action]] -> [[String]]
prepOutCSV connections paths =
    let header = ["v1", "v2", "path"]
        content = zipWith prepCSVRow connections paths
    in header:content
    where
        prepCSVRow :: Connection -> Maybe [Action] -> [String]
        prepCSVRow (Connection v1 v2) maybePath =
            let connectionStrings = [show v1, show v2]
                pathString = case maybePath of
                    Nothing -> "NA"
                    Just actions ->
                        let vertices = actionsToPath actions
                        in intercalate ";" $ map show vertices
            in connectionStrings ++ [pathString]

pathForConnections :: [Edge] -> [Connection] -> IO [Maybe [Action]]
pathForConnections edges [] = return []
pathForConnections edges ((Connection v1 v2):xs) = do
    path <- findBestPath edges v1 v2
    case path of
        Nothing -> do
            nextPaths <- pathForConnections edges xs
            return $ Nothing:nextPaths
        Just actions  -> do
            --putStrLn $ show actions
            let remainingEdges = filterEgdesByActions edges actions
            --putStrLn $ show remainingEdges
            nextPaths <- pathForConnections remainingEdges xs
            return $ (Just actions):nextPaths

findBestPath :: [Edge] -> Vertex -> Vertex -> IO (Maybe [Action])
findBestPath edges start end =
    let actions = concat $ map edgeToActions edges
    in getOneValue $ head $ sortByCost $ generatePaths actions [] start end 0 []

generatePaths :: [Action] -> [Vertex] ->  Vertex -> Vertex -> Int -> [Action] -> [[Action]]
generatePaths allActions visited current end steps acc
    | current == end = [reverse acc]
    | otherwise = do
        action <- validActions
        generatePaths
            allActions
            (current:visited) (getV2 action) end
            (steps + 1)
            (action:acc)
  where
      -- pruning mechanism
      validActions :: [Action]
      validActions = filter (\a -> isFromCurV a && isNotVisited a) allActions
      isFromCurV :: Action -> Bool
      isFromCurV (Action v1 _ _) = v1 == current
      isNotVisited :: Action -> Bool
      isNotVisited (Action _ v2 _) = not $ any (\v -> v2 == v) visited

actionsToPath :: [Action] -> [Vertex]
actionsToPath [] = []
actionsToPath (x:xs) = nub $ ([getV1 x, getV2 x] ++ (actionsToPath xs))

data Connection = Connection Vertex Vertex
    deriving Show
makeConnection :: String -> String -> Connection
makeConnection v1 v2 = Connection (read v1) (read v2)

data Edge = Edge Vertex Vertex Float -- v1 v2 cost
    deriving (Show, Eq)
makeEdge :: String -> String -> String -> Edge
makeEdge v1 v2 cost = Edge (read v1) (read v2) (read cost)

filterEgdesByActions :: [Edge] -> [Action] -> [Edge]
filterEgdesByActions edges actions = filter (\e -> not $ isEdgeUsedByAnyAction e actions) edges
isEdgeUsedByAnyAction :: Edge -> [Action] -> Bool
isEdgeUsedByAnyAction edge actions = any (isEdgeUsedByAction edge) actions
isEdgeUsedByAction :: Edge -> Action -> Bool
isEdgeUsedByAction (Edge ev1 ev2 _) (Action av1 av2 _) = (ev1 == av1 && ev2 == av2) || (ev1 == av2 && ev2 == av1)

data Action = Action Vertex Vertex Float -- v1 v2 cost
    deriving (Show, Eq)
edgeToActions :: Edge -> [Action]
edgeToActions (Edge v1 v2 cost) = [Action v1 v2 cost, Action v2 v1 cost]
getV1 :: Action -> Vertex
getV1 (Action v1 _ _) = v1
getV2 :: Action -> Vertex
getV2 (Action _ v2 _) = v2
getCost :: Action -> Float
getCost (Action _ _ c) = c
sumCosts :: [Action] -> Float
sumCosts xs = sum $ map getCost xs
minByCost :: [[Action]] -> [Action]
minByCost xss = minimumBy (\xs ys -> compare (sumCosts xs) (sumCosts ys)) xss
sortByCost :: [[Action]] -> [[Action]]
sortByCost xss = sortBy (\xs ys -> sumCosts xs < sumCosts ys) xss

type Vertex = Int



























