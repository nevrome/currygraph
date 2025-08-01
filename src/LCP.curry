module LCP where

import Types
import Parsers

import Data.Global
import System.IO
import System.IO.Unsafe
import Control.Search.AllValues

data LCPOptions = LCPOptions {
      lcpVertFile :: String
    , lcpEdgeFile :: String
    , lcpConnectionFile :: String
    , lcpOutFile :: String
} deriving Show

runLCP :: LCPOptions -> IO ()
runLCP (LCPOptions vertFile edgeFile connectionFile outFile) = do
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
    writePaths outFile connections paths
    putStrLn "Done"

pathForConnections :: [Edge] -> [Connection] -> Int -> IO [Maybe [Action]]
pathForConnections edges [] _ = return []
pathForConnections edges ((Connection v1 v2 sumCost):xs) step = do
    putStr (show step ++ ".")
    hFlush stdout -- write our immediatelly
    -- start computation
    path <- findBestPath edges v1 v2 sumCost
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
branchesExplored :: GlobalT Int
branchesExplored = globalT "Main.branchesExplored" 0

findBestPath :: [Edge] -> Vertex -> Vertex -> Float -> IO (Maybe [Action])
findBestPath edges start end sumCost = do
    writeGlobalT minCostDiscovered (sumCost*1.5)
    writeGlobalT branchesExplored 0
    let actions = concat $ map edgeToActions edges
    case isEndStillReachable end actions of
        False -> return Nothing
        True -> do
            maybeBestPath <- getOneValue $ head $ sortByCost $ generatePaths actions [] start end 0 0 []
            return maybeBestPath
    where
        isEndStillReachable :: Vertex -> [Action] -> Bool
        isEndStillReachable (Vertex v _ _ _) actions = any (\(Action _ (Vertex v2 _ _ _) _) -> v == v2) actions

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
          isFromCurV a &&
          isNotVisited a &&
          isCostAboveMinCostDiscovered a &&
          isBelowBranchLimit
      isFromCurV :: Action -> Bool
      isFromCurV (Action v1 _ _) = v1 == current
      isNotVisited :: Action -> Bool
      isNotVisited (Action _ v2 _) = not $ any (\v -> v2 == v) visited
      isCostAboveMinCostDiscovered :: Action -> Bool
      isCostAboveMinCostDiscovered (Action _ _ c) =
          let previousMinCost = unsafePerformIO $! readGlobalT minCostDiscovered
          in (cost + c) < previousMinCost
      isBelowBranchLimit :: Bool
      isBelowBranchLimit =
          let nrBranchesExplored = unsafePerformIO $! readGlobalT branchesExplored
              update = unsafePerformIO $ do
                  writeGlobalT branchesExplored (nrBranchesExplored + 1)
                  return()
          in update `seq` nrBranchesExplored < 1000
