module Types where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

infinity :: Float
infinity = 1.0 / 0.0

type VertexMap = M.Map Int Vertex

buildVertexMap :: [Vertex] -> VertexMap
buildVertexMap vertices = M.fromList $ map (\v@(Vertex i) -> (i, v)) vertices
findVertexUnsafe :: VertexMap -> Int -> Vertex
findVertexUnsafe vm voi = fromJust $ M.lookup voi vm

type AdjacencyMap = M.Map Vertex [(Vertex, Float)] -- (neighboring vertex, float)

buildAdjacencyMap :: [Edge] -> AdjacencyMap
buildAdjacencyMap es = foldl addEdge M.empty es
  where
    addEdge m (Edge v1 v2 c) = M.insertWith (++) v1 [(v2,c)] $ M.insertWith (++) v2 [(v1,c)] m
removeEdges :: AdjacencyMap -> [(Vertex,Vertex)] -> AdjacencyMap
removeEdges m [] = m
removeEdges m toRemove =
    let remE v1 v2 = M.adjust (filter ((/= v2) . fst)) v1
    in foldl (\m (vFrom,vTo) -> remE vTo vFrom (remE vFrom vTo m)) m toRemove
removeVertices :: AdjacencyMap -> [Vertex] -> AdjacencyMap
removeVertices m toRemove =
    let toRemoveSet = S.fromList toRemove
        -- drop neighbours that are in toRemoveSet
        filteredList =
            [ (k, filter (\(v,_) -> not (S.member v toRemoveSet)) nbrs)
            | (k, nbrs) <- M.toList m
            , not (S.member k toRemoveSet) -- also drop the keys
            ]
    in M.fromList filteredList

getNeighbors :: AdjacencyMap -> Vertex -> [Vertex]
getNeighbors adj v = map fst $ M.findWithDefault [] v adj
getNeighborsWithCost :: AdjacencyMap -> Vertex -> [(Vertex,Float)]
getNeighborsWithCost adj v = M.findWithDefault [] v adj


data Edge = Edge Vertex Vertex Float -- v1 v2 cost
    deriving (Show, Eq)
makeEdge :: Vertex -> Vertex -> String -> Edge
makeEdge v1 v2 cost = Edge v1 v2 (read cost)

data Connection = Connection Vertex Vertex Float -- v1 v2 sum_cost
    deriving Show
makeConnection :: Vertex -> Vertex -> String -> Connection
makeConnection v1 v2 sumCost = Connection v1 v2 (read sumCost)

data Vertex = Vertex Int

instance Show Vertex where
    show (Vertex v) = show v
instance Eq Vertex where
    (Vertex v1) == (Vertex v2) = v1 == v2
instance Ord Vertex where
  compare (Vertex v1) (Vertex v2) = compare v1 v2
makeVertex :: String -> Vertex
makeVertex v = Vertex (read v)
