module Draw where

import Types
import Parsers

import Data.List
import qualified Data.GraphViz as GV
import System.IO
import qualified Data.Map as M

data DrawOptions = DrawOptions {
      drawVertFile :: String
    , drawEdgeFile :: String
    , drawPathFile :: Maybe String
    , drawOutFile :: String
} deriving Show

runDraw :: DrawOptions -> IO ()
runDraw (
    DrawOptions
    vertFile edgeFile maybePathFile outFile
    ) = do
    putStrLn "Reading data..."
    vertices <- readVertices vertFile
    let vm = buildVertexMap vertices
    putStrLn $ "Vertices: " ++ show (M.size vm)
    edges <- readEdges edgeFile vm
    putStrLn $ "Edges: " ++ show (length edges)
    paths <- case maybePathFile of
        Nothing -> return []
        Just pathFile -> do
            ps <- readPaths pathFile vm
            putStrLn $ "Paths: " ++ show (length ps)
            return ps
    -- mapM_ (putStrLn . show) paths
    -- putStrLn $ show $ nodePathAttributes paths (vertices !! 300)
    putStrLn "Preparing GraphViz data types..."
    let gvNodes = map (vertex2GVNode paths) vertices
        -- gvEdges = map (edge2GVEdge paths) edges
        gvEdges = map edge2GVEdge edges
        gvUGraph = GV.ugraph "currygraph" gvNodes gvEdges
    putStrLn "Writing GraphViz file..."
    let dotString = GV.showDotGraph gvUGraph
    h <- openFile outFile WriteMode
    hPutStrLn h dotString
    hFlush h
    hClose h
    putStrLn "Done"

pathName :: Path -> String
pathName (Path [] _) = "path_empty"
pathName (Path (first:rest) _) =
    "path_" ++ show first ++ "_" ++ show (lastD first rest)

-- last from the prelude is not deterministic
lastD :: a -> [a] -> a
lastD current [] = current
lastD _ (x:xs) = lastD x xs

nodePathAttributes :: [Path] -> Vertex -> [(String, String)]
nodePathAttributes paths vertex =
    [ (pathName path, "true") | path <- paths, isVertexInPath path vertex ]

edgePathAttributes :: [Path] -> Edge -> [(String, String)]
edgePathAttributes paths edge =
    [ (pathName path, "true") | path <- paths, isEdgeInPath path edge ]

-- vertex2GVNode :: Vertex -> GV.Node
-- vertex2GVNode (Vertex i Nothing) =
--     GV.Node (show i) []
-- vertex2GVNode (Vertex i (Just (SpatPos long lat))) =
--     GV.Node (show i) [("long", show long), ("lat", show lat)]

vertex2GVNode :: [Path] -> Vertex -> GV.Node
vertex2GVNode paths vertex@(Vertex i Nothing) =
    GV.Node (show i)
            (nodePathAttributes paths vertex)
vertex2GVNode paths vertex@(Vertex i (Just (SpatPos long lat))) =
    GV.Node (show i)
            ( [ ("long", show long), ("lat", show lat) ]
             ++ nodePathAttributes paths vertex )

edge2GVEdge :: Edge -> GV.Edge
edge2GVEdge (Edge (Vertex i1 _) (Vertex i2 _) c) =
    GV.Edge (show i1) (show i2) [("cost", show c)]

-- edge2GVEdge :: [Path] -> Edge -> GV.Edge
-- edge2GVEdge paths edge@(Edge (Vertex i1 _) (Vertex i2 _) c) =
--     GV.Edge (show i1) (show i2)
--             ( [ ("cost", show c) ]
--             ++ edgePathAttributes paths edge )
