module LCP where

import Types
import Parsers

import Data.Global
import System.IO
import System.IO.Unsafe
import Control.Search.AllValues
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

data LCPOptions = LCPOptions {
      lcpVertFile :: String
    , lcpEdgeFile :: String
    , lcpConnectionFile :: String
    , lcpDeleteUsedEdges :: Bool
    , lcpMaxNrBranches :: Int
    , lcpCostThreshold :: CostThreshold
    , lcpUpdateCostThreshold :: Bool
    , lcpOutFile :: String
} deriving Show

data CostThreshold = None | Absolute Float | Relative Float
    deriving Show

runLCP :: LCPOptions -> IO ()
runLCP (
    LCPOptions
    vertFile edgeFile connectionFile
    deleteUsed maxNrBranches costThreshold updateCostThreshold
    outFile
    ) = do
    putStrLn "Reading data..."
    vertices <- readVertices vertFile
    let vm = buildVertexMap vertices
    putStrLn $ "Vertices: " ++ show (M.size vm)
    edges <- readEdges edgeFile vm
    putStrLn $ "Edges: " ++ show (length edges)
    let actions = concat $ map edgeToActions edges
    putStrLn $ "Actions: " ++ show (length actions)
    connections <- readConnections connectionFile vm
    putStrLn $ "Connections: " ++ show (length connections)
    putStrLn "Searching..."
    h <- openFile outFile WriteMode
    hPutStrLn h "v1,v2,initial_sum_cost,path" -- csv header
    pathForConnections h actions connections deleteUsed maxNrBranches costThreshold updateCostThreshold
    hFlush h
    hClose h
    putStrLn "Done"

pathForConnections :: Handle -> [Action] -> [Connection] -> Bool -> Int -> CostThreshold -> Bool -> IO ()
pathForConnections _ _ [] _ _ _ _ = return ()
pathForConnections h allActions (con@(Connection v1 v2 sumCost):xs)
                   deleteUsed maxNrBranches
                   costThreshold updateCostThreshold = do
    path <- findBestPath allActions v1 v2 sumCost maxNrBranches costThreshold updateCostThreshold
    writeConnectionResult h con path
    let remainingActions = case deleteUsed of
            True -> case path of
                Nothing -> allActions
                Just actions -> filterActions allActions actions
            False -> allActions
    pathForConnections h remainingActions xs deleteUsed maxNrBranches costThreshold updateCostThreshold

writeConnectionResult :: Handle -> Connection -> Maybe [Action] -> IO ()
writeConnectionResult h (Connection v1 v2 sumCost) maybeActions = do
    let pathString = case maybeActions of
            Nothing -> "NA"
            Just actions ->
                let vertices = actionsToPath actions
                in intercalate ";" $ map show vertices
        row = intercalate "," [show v1, show v2, show sumCost, pathString]
    hPutStrLn h row

actionsToPath :: [Action] -> [Vertex]
actionsToPath [] = []
actionsToPath (x:xs) = nub ([getV1 x, getV2 x] ++ (actionsToPath xs))

-- global mutable variable to keep track of the cheapest path already discovered
currentCostThreshold :: GlobalT Float
currentCostThreshold = globalT "Main.currentCostThreshold" 0
branchesExplored :: GlobalT Int
branchesExplored = globalT "Main.branchesExplored" 0

findBestPath :: [Action] -> Vertex -> Vertex
                -> Float -> Int -> CostThreshold
                -> Bool -> IO (Maybe [Action])
findBestPath actions start end sumCost maxNrBranches costThreshold updateCostThreshold = do
    case costThreshold of
        None -> writeGlobalT currentCostThreshold infinity
        Absolute x -> writeGlobalT currentCostThreshold x
        Relative x -> writeGlobalT currentCostThreshold (sumCost*x)
    writeGlobalT branchesExplored 0
    case isEndStillReachable end actions of
        False -> return Nothing
        True -> do
            maybeBestPath <- getOneValue $ head $ sortByCost $ generatePaths actions maxNrBranches updateCostThreshold end S.empty start 0 []
            return maybeBestPath
    where
        isEndStillReachable :: Vertex -> [Action] -> Bool
        isEndStillReachable (Vertex v _ _ ) actions = any (\(Action _ (Vertex v2 _ _ ) _) -> v == v2) actions

generatePaths :: [Action] -> Int -> Bool -> Vertex -> S.Set Vertex -> Vertex -> Float -> [Action] -> [[Action]]
generatePaths allActions maxNrBranches updateCostThreshold end visited current cost acc
    | current == end && updateCostThreshold =
        let update = unsafePerformIO $ do
                writeGlobalT currentCostThreshold cost
                return ()
        in update `seq` [reverse acc]
    | current == end = [reverse acc]
    | otherwise = do
        action <- validActions
        generatePaths
            allActions maxNrBranches updateCostThreshold end
            (S.insert current visited) (getV2 action)
            (cost + getCost action)
            (action:acc)
  where
      -- pruning mechanism
      validActions :: [Action]
      validActions = take 3 $ sortBySpatialDistToDest end $ filter checkAction allActions
      checkAction :: Action -> Bool
      checkAction a =
          isFromCurV a &&
          isNotVisited a &&
          isCostAboveCostThreshold a &&
          isBelowBranchLimit
      isFromCurV :: Action -> Bool
      isFromCurV (Action v1 _ _) = v1 == current
      isNotVisited :: Action -> Bool
      isNotVisited (Action _ v2 _) =  not $ S.member v2 visited
      isCostAboveCostThreshold :: Action -> Bool
      isCostAboveCostThreshold (Action _ _ c) =
          let previousMinCost = unsafePerformIO $! readGlobalT currentCostThreshold
          in (cost + c) < previousMinCost
      isBelowBranchLimit :: Bool
      isBelowBranchLimit =
          let nrBranchesExplored = unsafePerformIO $! readGlobalT branchesExplored
              update = unsafePerformIO $ do
                  writeGlobalT branchesExplored (nrBranchesExplored + 1)
                  return()
          in update `seq` nrBranchesExplored < maxNrBranches
