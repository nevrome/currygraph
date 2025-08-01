module Types where

import Data.List
import Data.Maybe

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

data Connection = Connection Vertex Vertex Float -- v1 v2 sum_cost
    deriving Show
makeConnection :: Vertex -> Vertex -> String -> Connection
makeConnection v1 v2 sumCost = Connection v1 v2 (read sumCost)

data Vertex = Vertex Int Float Float Bool -- v long lat focal
instance Show Vertex where
    show (Vertex v _ _ _) = show v
instance Eq Vertex where
    (Vertex v1 _ _ _) == (Vertex v2 _ _ _) = v1 == v2
makeVertex :: String -> String -> String -> String -> Vertex
makeVertex v long lat focal = Vertex (read v) (read long) (read lat) (read focal)
findVertexByID :: [Vertex] -> Int -> Vertex
findVertexByID xs voi = fromJust $ find (\(Vertex id _ _ _) -> id == voi) xs

distHaversine :: Vertex -> Vertex -> Float
distHaversine (Vertex _ long1 lat1 _) (Vertex _ long2 lat2 _) =
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
