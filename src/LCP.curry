module LCP where

import Types
import Parsers

import System.IO
import Data.List
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Search.AllValues
import System.Random

data LCPOptions = LCPOptions {
      lcpVertFile :: String
    , lcpEdgeFile :: String
    , lcpConnectionFile :: String
    , lcpOmissionStrategy :: OmissionStrategy
    , lcpDestFile :: Maybe String
    , lcpOutFile :: String
}

data OmissionStrategy = OmitNone | OmitDests | Filter

runLCP :: LCPOptions -> IO ()
runLCP (
    LCPOptions
    vertFile edgeFile connectionFile
    omissionStrategy maybeDestFile
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
    putStrLn $ "Size adjacency map: " ++ show (M.size adj) -- to force evaluation
    connections <- readConnections connectionFile vm
    putStrLn $ "Connections: " ++ show (length connections)
    dests <- case maybeDestFile of
        Nothing -> return S.empty
        Just destFile -> do
            verticesDest <- readVertices destFile
            let verticesDestSet = S.fromList verticesDest
            putStrLn $ "Destination vertices: " ++ show (S.size verticesDestSet)
            return verticesDestSet
    putStrLn "Searching..."
    h <- openFile outFile WriteMode
    hPutStrLn h "v1,v2,sum_cost,path" -- csv header
    pathsForConnections h adj connections omissionStrategy dests
    hFlush h
    hClose h
    putStrLn "Done"

pathsForConnections :: Handle -> AdjacencyMap -> [Connection] -> OmissionStrategy -> (S.Set Vertex) -> IO ()
pathsForConnections h adj cons omissionStrategy dests =
    (flip mapM_) cons $ \con -> do
        print con
        hFlush stdout
        case pathForConnection adj omissionStrategy dests con of
            Nothing   -> pure ()
            Just path -> writePath h con path

pathForConnection :: AdjacencyMap -> OmissionStrategy -> S.Set Vertex -> Connection -> Maybe Path
pathForConnection adj omissionStrategy dests con@(Connection start end) =
    case omissionStrategy of
        OmitNone  -> dijkstra adj con S.empty
        OmitDests -> dijkstra adj con toOmit
        Filter    -> dijkstra adj con S.empty >>= rejectInHindsight
  where
    toOmit = dests `S.difference` S.fromList [start, end]
    rejectInHindsight path
        | shouldBeOmitted toOmit path = Nothing
        | otherwise                   = Just path
    shouldBeOmitted :: (S.Set Vertex) -> Path -> Bool
    shouldBeOmitted toOmit (Path vs _) = any (\v -> S.member v toOmit) vs

dijkstra :: AdjacencyMap -> Connection -> S.Set Vertex -> Maybe Path
dijkstra adj (Connection start end) toOmit = go [(start,0,[start])] S.empty
  where
    go [] _ = Nothing
    go ((curPos,curCost,curPath):queue) visited
      | curPos == end = Just $ Path (reverse curPath) curCost
      | curPos `S.member` visited = go queue visited
      | otherwise =
          let neighbors     = getNeighborsWithCost adj curPos
              --updatedQueue = foldl update queue neighbors
              neighborsOmit = filter (\(v,_) -> not (S.member v toOmit)) neighbors
              updatedQueue = foldl update queue neighborsOmit
          in go updatedQueue (S.insert curPos visited)
          where
              update :: [(Vertex,Float,[Vertex])] -> (Vertex,Float) -> [(Vertex,Float,[Vertex])]
              update accQueue (neighborVertex, edgeWeight)
                | neighborVertex `S.member` visited = accQueue
                | otherwise = insertBy (\(_,c1,_) (_,c2,_) -> c1 < c2)
                                       (neighborVertex, curCost+edgeWeight, neighborVertex:curPath)
                                       accQueue

writePath :: Handle -> Connection -> Path -> IO ()
writePath h (Connection v1 v2) (Path vs cost) =
    hPutStrLn h $ intercalate "," [show v1, show v2, show cost, showPath vs]
showPath :: [Vertex] -> String
showPath = intercalate ";" . map show
