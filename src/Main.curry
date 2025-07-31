module Main where

import Data.List
import Data.Maybe
import qualified OptParse as OP
import System.Environment
import Control.Search.AllValues
import Text.CSV (readCSVFile, writeCSVFile)
import Data.Global
import System.IO
import System.IO.Unsafe

data Options = Options {
      vertFile :: String
    , edgeFile :: String
    , connectionFile :: String
    , outFile :: String
} deriving Show

cmdParser = OP.optParser $
    OP.option (\s a -> a { vertFile = s }) (
        OP.long "vertFile"
        OP.<> OP.short "v"
        OP.<> OP.metavar "PATH"
        OP.<> OP.help "..."
    ) OP.<.> OP.option (\s a -> a { edgeFile = s }) (
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
        defaultOpts = Options "" "" "" ""

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
readVertices :: String -> IO [Vertex]
readVertices path = do
    header:rows <- readCSVFile path
    let colID = getCol "id" header rows
        colLong = getCol "long" header rows
        colLat = getCol "lat" header rows
    let vertices = zipWith3 makeVertex colID colLong colLat
    return vertices

readEdges :: String -> [Vertex] -> IO [Edge]
readEdges path vertices = do
    header:rows <- readCSVFile path
    let colV1 = getCol "v1" header rows
        verticesV1 = map (findVertexByID vertices . read) colV1
        colV2 = getCol "v2" header rows
        verticesV2 = map (findVertexByID vertices . read) colV2
        colCost = getCol "cost" header rows
    let edges = zipWith3 makeEdge verticesV1 verticesV2 colCost
    return edges
    
readConnections :: String -> [Vertex] -> IO [Connection]
readConnections path vertices = do
    header:rows <- readCSVFile path
    let colV1 = getCol "v1" header rows
        verticesV1 = map (findVertexByID vertices . read) colV1
        colV2 = getCol "v2" header rows
        verticesV2 = map (findVertexByID vertices . read) colV2
    let connections = zipWith makeConnection verticesV1 verticesV2
    return connections

getCol :: String -> [String] -> [[String]] -> [String]
getCol colName header rows =
    let colNum = getColNum colName header
    in map (\row -> row !! colNum) rows
    where
        getColNum :: String -> [String] -> Int
        getColNum colName header = fromJust $ findIndex (\x -> x == colName) header 

runCNN :: Options -> IO ()
runCNN (Options vertFile edgeFile connectionFile outFile) = do
    putStrLn "Reading data..."
    vertices <- readVertices vertFile
    putStrLn $ "Vertices: " ++ show (length vertices)
    edges <- readEdges edgeFile vertices
    putStrLn $ "Edges: " ++ show (length edges)
    connections <- readConnections connectionFile vertices
    putStrLn $ "Connections: " ++ show (length connections)
    putStrLn "Searching paths..."
    paths <- pathForConnections edges connections 0
    putStrLn "Writing output..."
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

pathForConnections :: [Edge] -> [Connection] -> Int -> IO [Maybe [Action]]
pathForConnections edges [] _ = return []
pathForConnections edges ((Connection v1 v2):xs) step = do
    putStr (show step ++ ".")
    hFlush stdout -- write our immediatelly
    -- start computation
    path <- findBestPath edges v1 v2
    case path of
        Nothing -> do
            nextPaths <- pathForConnections edges xs (step+1)
            return $ Nothing:nextPaths
        Just actions  -> do
            --putStrLn $ show actions
            let remainingEdges = filterEgdesByActions edges actions
            --putStrLn $ show remainingEdges
            nextPaths <- pathForConnections remainingEdges xs (step+1)
            return $ (Just actions):nextPaths

-- global mutable variable to keep track of the cheapest path already discovered
minCostDiscovered :: GlobalT Float
minCostDiscovered = globalT "Main.minCostDiscovered" 10000

findBestPath :: [Edge] -> Vertex -> Vertex -> IO (Maybe [Action])
findBestPath edges start end = do
    writeGlobalT minCostDiscovered 10000
    let actions = concat $ map edgeToActions edges
    maybeBestPath <- getOneValue $ head $ sortByCost $ generatePaths actions [] start end 0 0 []
    return maybeBestPath

generatePaths :: [Action] -> [Vertex] ->  Vertex -> Vertex -> Int -> Float -> [Action] -> [[Action]]
generatePaths allActions visited current end steps cost acc
    | current == end =
        let update = unsafePerformIO $ do
                writeGlobalT minCostDiscovered cost
                return ()
        in update `seq` [reverse acc]
    | otherwise = do
        action <- validActions
        generatePaths
            allActions
            (current:visited) (getV2 action) end
            (steps + 1)
            (cost + getCost action)
            (action:acc)
  where
      -- pruning mechanism
      validActions :: [Action]
      validActions = sortBySpatialDistToDest end $ filter checkAction allActions
      checkAction :: Action -> Bool
      checkAction a =
          --isNotTooManySteps &&
          isFromCurV a
          && isNotVisited a
          && isCostAboveMinCostDiscovered a
      isFromCurV :: Action -> Bool
      isFromCurV (Action v1 _ _) = v1 == current
      isNotVisited :: Action -> Bool
      isNotVisited (Action _ v2 _) = not $ any (\v -> v2 == v) visited
      isNotTooManySteps :: Bool
      isNotTooManySteps = steps < 6
      isCostAboveMinCostDiscovered :: Action -> Bool
      isCostAboveMinCostDiscovered (Action _ _ c) =
          let previousMinCost = unsafePerformIO $! readGlobalT minCostDiscovered
          in (cost + c) < previousMinCost

actionsToPath :: [Action] -> [Vertex]
actionsToPath [] = []
actionsToPath (x:xs) = nub $ ([getV1 x, getV2 x] ++ (actionsToPath xs))

data Edge = Edge Vertex Vertex Float -- v1 v2 cost
    deriving (Show, Eq)
makeEdge :: Vertex -> Vertex -> String -> Edge
makeEdge v1 v2 cost = Edge v1 v2 (read cost)

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

sortBySpatialDistToDest :: Vertex -> [Action] -> [Action]
sortBySpatialDistToDest dest xs = sortBy (\x y -> distToDest dest x < distToDest dest y) xs
distToDest :: Vertex -> Action -> Float
distToDest dest (Action _ v2 _) = distHaversine dest v2

data Connection = Connection Vertex Vertex
    deriving Show
makeConnection :: Vertex -> Vertex -> Connection
makeConnection v1 v2 = Connection v1 v2

data Vertex = Vertex Int Float Float -- v long lat
instance Show Vertex where
    show (Vertex v _ _) = show v
instance Eq Vertex where
    (Vertex v1 _ _) == (Vertex v2 _ _) = v1 == v2
makeVertex :: String -> String -> String -> Vertex
makeVertex v long lat = Vertex (read v) (read long) (read lat)
findVertexByID :: [Vertex] -> Int -> Vertex
findVertexByID xs voi = fromJust $ find (\(Vertex id _ _) -> id == voi) xs

distHaversine :: Vertex -> Vertex -> Float
distHaversine (Vertex _ long1 lat1) (Vertex _ long2 lat2) =
    sqrt ((long1 - long2)^2 + (lat1 - lat2)^2)
    --let r = 6371000  -- radius of Earth in metres
    --    toRadians n = n * pi / 180
    --    square x = x * x
    --    cosr = cos . toRadians
    --    dlat = toRadians (lat1 - lat2) / 2
    --    dlong = toRadians (long1 - long2) / 2
    --    a = square (sin dlat) + cosr lat1 * cosr lat2 * square (sin dlong)
    --    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    --in r * c



















