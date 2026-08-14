module VORO where

import Types
import Parsers

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)
import System.IO
import Data.List

data VOROOptions = VOROOptions {
      voroVertFile :: FilePath
    , voroEdgeFile :: FilePath
    , voroDestFile :: FilePath
    , voroOutFile  :: FilePath
} deriving Show

runVORO :: VOROOptions -> IO ()
runVORO (
    VOROOptions
    vertFile edgeFile destFile
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
    putStrLn $ "Size adjacency map: " ++ show (M.size adj)
    verticesDest <- readVertices destFile
    putStrLn $ "Destination vertices: " ++ show (length verticesDest)
    putStrLn "Running multi-source Dijkstra..."
    let voronoi = multiSourceDijkstra adj verticesDest
    putStrLn "Detecting adjacent Voronoi cells..."
    let candidates = voronoiConnections adj voronoi
    putStrLn $ "Found Connections: " ++ show (length candidates)
    putStrLn "Sorting by cost and vertex id..."
    let sortedCandidates = sortByCost candidates
    putStrLn "Writing connections..."
    writeConnections outFile sortedCandidates
    putStrLn "Done"

-- result of the multi-source shortest-path search
-- for every graph vertex:
-- - ownerOf[v]    = nearest destination
-- - distanceOf[v] = distance to that destination
-- - parentOf[v]   = next vertex toward that destination
data Voronoi = Voronoi
    { ownerOf    :: M.Map Vertex Vertex
    , distanceOf :: M.Map Vertex Float
    , parentOf   :: M.Map Vertex Vertex
    } deriving Show

-- a connection induced by an edge crossing between two Voronoi cells:
-- boundaryFrom and boundaryTo are the endpoints of the original graph
-- edge at which the two cells meet
data ConnectionCandidate = ConnectionCandidate
    { connectionFrom :: Vertex
    , connectionTo   :: Vertex
    , connectionCost :: Float
    , boundaryFrom   :: Vertex
    , boundaryTo     :: Vertex
    } deriving Show

sortByCost :: [ConnectionCandidate] -> [ConnectionCandidate]
sortByCost xs = sortBy compareByCost xs
  where
    compareByCost (ConnectionCandidate from1 to1 c1 _ _)
                  (ConnectionCandidate from2 to2 c2 _ _) =
         c1 < c2
      || (c1 == c2 && from1 < from2)
      || (c1 == c2 && from1 == from2 && to1 <= to2)

-- effectively a BFS from all destination vertices at the same time
multiSourceDijkstra :: AdjacencyMap -> [Vertex] -> Voronoi
multiSourceDijkstra adj destinations =
    let initialQueue     = S.fromList [(0, d) | d <- destinations]
        initialOwners    = M.fromList [(d, d) | d <- destinations]
        initialDistances = M.fromList [(d, 0) | d <- destinations]
    in go initialQueue initialOwners initialDistances M.empty
  where
    go queue owners distances parents =
        case setMinView queue of
            Nothing -> Voronoi owners distances parents
            Just ((currentDistance, current), queueWithoutCurrent) ->
                case M.lookup current distances of
                    Nothing -> error "multiSourceDijkstra: queued vertex has no recorded distance"
                    Just bestKnownDistance
                        | currentDistance > bestKnownDistance -> go queueWithoutCurrent owners distances parents
                        | otherwise ->
                            let currentOwner = fromJust $ M.lookup current owners
                                neighbors = getNeighborsWithCost adj current
                                (newQueue, newOwners, newDistances, newParents) =
                                    foldl (assignNeighborToRegion current currentOwner currentDistance)
                                          (queueWithoutCurrent, owners, distances, parents) neighbors
                            in go newQueue newOwners newDistances newParents
    assignNeighborToRegion current currentOwner currentDistance (queue, owners, distances, parents) (neighbor, edgeCost) =
        let alternativeDistance = currentDistance + edgeCost
        in case M.lookup neighbor distances of
            Nothing ->
                ( S.insert (alternativeDistance, neighbor) queue
                , M.insert neighbor currentOwner owners
                , M.insert neighbor alternativeDistance distances
                , M.insert neighbor current parents
                )
            Just oldDistance
                | alternativeDistance < oldDistance ->
                    ( S.insert (alternativeDistance, neighbor) queue
                    , M.insert neighbor currentOwner owners
                    , M.insert neighbor alternativeDistance distances
                    , M.insert neighbor current parents
                    )
                | otherwise -> (queue, owners, distances, parents)

setMinView :: Ord a => S.Set a -> Maybe (a, S.Set a)
setMinView set =
    case S.toList set of -- toList returns an ordered (!) list, so that's why x is the minimum
        []  -> Nothing
        x:_ -> Just (x, S.delete x set)

-- inspect every graph edge: if its endpoints have different owners, the corresponding destination cells meet
voronoiConnections :: AdjacencyMap -> Voronoi -> [ConnectionCandidate]
voronoiConnections adj voronoi = M.elems $ foldl addVertexEdges M.empty (M.toList adj)
  where
    addVertexEdges candidates (u, neighbors) = foldl (addCrossing u) candidates neighbors
    addCrossing u candidates (v, edgeCost) =
        case ( M.lookup u (ownerOf voronoi)
             , M.lookup v (ownerOf voronoi)
             , M.lookup u (distanceOf voronoi)
             , M.lookup v (distanceOf voronoi)
             ) of
            (Just ownerU, Just ownerV, Just distanceU, Just distanceV) | ownerU /= ownerV ->
                let candidateCost = distanceU + edgeCost + distanceV
                    candidate = makeCandidate ownerU ownerV candidateCost u v
                    key = canonicalPair ownerU ownerV
                in M.insertWith chooseBetter key candidate candidates
            _ -> candidates
    chooseBetter new old
        | connectionCost new < connectionCost old = new
        | otherwise                               = old

-- helpers to ensure that (a,b) and (b,a) are treated as the same connection
canonicalPair :: Vertex -> Vertex -> (Vertex, Vertex)
canonicalPair a b | a <= b    = (a, b)
                  | otherwise = (b, a)

makeCandidate :: Vertex -> Vertex -> Float -> Vertex -> Vertex -> ConnectionCandidate
makeCandidate ownerU ownerV cost u v
    | ownerU <= ownerV =
        ConnectionCandidate
            { connectionFrom = ownerU
            , connectionTo   = ownerV
            , connectionCost = cost
            , boundaryFrom   = u
            , boundaryTo     = v
            }
    | otherwise =
        ConnectionCandidate
            { connectionFrom = ownerV
            , connectionTo   = ownerU
            , connectionCost = cost
            , boundaryFrom   = v
            , boundaryTo     = u
            }
            
writeConnections :: FilePath -> [ConnectionCandidate] -> IO ()
writeConnections outFile candidates =
    withFile outFile WriteMode $ \h -> do
        hPutStrLn h "v1,v2,sum_cost"
        mapM_ (\candidate -> hPutStrLn h $
                show (connectionFrom candidate) ++ "," ++
                show (connectionTo candidate)   ++ "," ++
                show (connectionCost candidate)) candidates
