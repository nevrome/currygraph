module Main where

import Data.List
import Data.Maybe
import qualified OptParse as OP
import System.Environment
import Control.Search.AllValues
import Text.CSV (readCSVFile)

data Options = Options {
      edgeFile :: String
    , startVertex :: Int
    , endVertex :: Int
} deriving Show

cmdParser = OP.optParser $
    OP.option (\s a -> a { edgeFile = s }) (
        OP.long "edgeFile"
        OP.<> OP.short "e"
        OP.<> OP.metavar "PATH"
        OP.<> OP.help "..."
    ) OP.<.> OP.option (\s a -> a { startVertex = read s }) (
        OP.long "startVertex"
        OP.<> OP.short "a"
        OP.<> OP.metavar "INT"
        OP.<> OP.help "..."
    ) OP.<.> OP.option (\s a -> a { endVertex = read s }) (
        OP.long "endVertex"
        OP.<> OP.short "b"
        OP.<> OP.metavar "INT"
        OP.<> OP.help "..."
    )

applyParse :: [Options -> Options] -> Options
applyParse fs = foldl (flip apply) defaultOpts fs
    where
        defaultOpts = Options "" 0 0

main :: IO ()
main = do
  args <- getArgs
  parseResult <- return $ OP.parse (intercalate " " args) cmdParser "cnn"
  case parseResult of
    Left err -> putStrLn err
    Right  v -> do
        let options = applyParse v
        runCNN options

runCNN :: Options -> IO ()
runCNN (Options edgeFile startVertex endVertex) = do
    header:rows <- readCSVFile edgeFile
    let colV1 = getCol "v1" header rows
        colV2 = getCol "v2" header rows
        colCost = getCol "cost" header rows
    let edges = zipWith3 makeEdge colV1 colV2 colCost
        actions = concat $ map edgeToActions edges
    --putStrLn $ show actions
    -- search
    maybeBestNPaths <- getOneValue $ take 1 $ sortByCost $ generatePaths actions [] startVertex endVertex 0 []
    putStrLn $ show maybeBestNPaths

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

data Edge = Edge Vertex Vertex Float -- v1 v2 cost
    deriving (Show, Eq)
makeEdge :: String -> String -> String -> Edge
makeEdge v1 v2 cost = Edge (read v1) (read v2) (read cost)

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

getCol :: String -> [String] -> [[String]] -> [String]
getCol colName header rows =
    let colNum = getColNum colName header
    in map (\row -> row !! colNum) rows
    where
        getColNum :: String -> [String] -> Int
        getColNum colName header = fromJust $ findIndex (\x -> x == colName) header  


























