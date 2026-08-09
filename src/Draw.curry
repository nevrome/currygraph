module Draw where

import Types
import Parsers

import qualified Data.GraphViz as GV
import System.IO
import qualified Data.Map as M

data DrawOptions = DrawOptions {
      drawVertFile :: String
    , drawEdgeFile :: String
    , drawOutFile :: String
} deriving Show

runDraw :: DrawOptions -> IO ()
runDraw (
    DrawOptions
    vertFile edgeFile outFile
    ) = do
    putStrLn "Reading data..."
    vertices <- readVertices vertFile
    let vm = buildVertexMap vertices
    putStrLn $ "Vertices: " ++ show (M.size vm)
    edges <- readEdges edgeFile vm
    putStrLn $ "Edges: " ++ show (length edges)
    putStrLn "Preparing GraphViz data types..."
    let gvNodes = map vertex2GVNode vertices
        gvEdges = map edge2GVEdge edges
        gvUGraph = GV.ugraph "currygraph" gvNodes gvEdges
    putStrLn "Writing GraphViz file..."
    let dotString = GV.showDotGraph gvUGraph
    h <- openFile outFile WriteMode
    hPutStrLn h dotString
    hFlush h
    hClose h
    putStrLn "Done"

vertex2GVNode :: Vertex -> GV.Node
vertex2GVNode (Vertex i Nothing) =
    GV.Node (show i) []
vertex2GVNode (Vertex i (Just (SpatPos long lat))) =
    GV.Node (show i) [("long", show long), ("lat", show lat)]

edge2GVEdge :: Edge -> GV.Edge
edge2GVEdge (Edge (Vertex i1 _) (Vertex i2 _) c) =
    GV.Edge (show i1) (show i2) [("cost", show c)]


