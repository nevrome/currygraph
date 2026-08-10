module Main where

import LCP
import BFS
import LRW
import Draw

import Data.List
import Data.Maybe
import OptParse ((<.>))
import qualified OptParse as OP
import System.Environment
import Data.Global
import System.IO
import System.IO.Unsafe

data Options = Options { com :: Command }
data Command =
      NoCommand
    | LCP LCPOptions
    | BFS BFSOptions
    | LRW LRWOptions
    | Draw DrawOptions

defaultOptions :: Options
defaultOptions = Options NoCommand

applyEither :: [Options -> Either String Options] -> Options -> Either String Options
applyEither [] z = Right z
applyEither (f:fs) z = case f z of
  Left err -> Left err
  Right z' -> applyEither fs z'
applyParse :: [Options -> Either String Options] -> Either String Options
applyParse fs = applyEither fs defaultOptions

main :: IO ()
main = do
  args <- getArgs
  parseResult <- return $ OP.parse (intercalate " " args) cmdParser "currygraph"
  case parseResult of
    Left err -> putStrLn err
    Right  r -> 
        case applyParse r of
              Left err   -> do putStrLn err
              Right opts -> runWithArgs opts
    
runWithArgs :: Options -> IO ()
runWithArgs (Options (LCP opts))  = runLCP opts
runWithArgs (Options (BFS opts))  = runBFS opts
runWithArgs (Options (LRW opts))  = runLRW opts
runWithArgs (Options (Draw opts)) = runDraw opts

-- generic parser constructors for command-specific options.
optionFor inject project update doc =
  OP.option
    (\value allOpts ->
      Right $ allOpts {
        com = inject (update value (project allOpts))
      })
    doc

flagFor inject project update doc =
  OP.flag
    (\allOpts ->
      Right $ allOpts {
        com = inject (update (project allOpts))
      })
    doc

lcpOption  = optionFor LCP  lcpOpts
bfsOption  = optionFor BFS  bfsOpts
lrwOption  = optionFor LRW  lrwOpts
drawOption = optionFor Draw drawOpts
lcpFlag  = flagFor LCP  lcpOpts
bfsFlag  = flagFor BFS  bfsOpts
lrwFlag  = flagFor LRW  lrwOpts
drawFlag = flagFor Draw drawOpts


-- parsers

-- general
docVertFile :: OP.Mod
docVertFile = OP.long "vertFile"
            OP.<> OP.short "v"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each vertex, columns: id, long, lat."
parseVertFileLCP  = lcpOption (\s o -> o { lcpVertFile = s }) docVertFile
parseVertFileBFS  = bfsOption (\s o -> o { bfsVertFile = s }) docVertFile
parseVertFileLRW  = lrwOption (\s o -> o { lrwVertFile = s }) docVertFile
parseVertFileDraw = drawOption (\s o -> o { drawVertFile = s }) docVertFile

docFocalVertFile :: OP.Mod
docFocalVertFile = OP.long "focalVertFile"
            OP.<> OP.short "f"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each vertex, columns: id, long, lat."
parseFocalVertFileLRW = lrwOption (\s o -> o { lrwFocalVertFile = s }) docFocalVertFile

docEdgeFile :: OP.Mod
docEdgeFile = OP.long "edgeFile"
            OP.<> OP.short "e"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each edge, columns: v1, v2, cost."
parseEdgeFileLCP = lcpOption (\s o -> o { lcpEdgeFile = s }) docEdgeFile
parseEdgeFileBFS = bfsOption (\s o -> o { bfsEdgeFile = s }) docEdgeFile
parseEdgeFileLRW = lrwOption (\s o -> o { lrwEdgeFile = s }) docEdgeFile
parseEdgeFileDraw = drawOption (\s o -> o { drawEdgeFile = s }) docEdgeFile

docPathFile :: OP.Mod
docPathFile = OP.long "pathFile"
            OP.<> OP.short "p"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each path, columns: path, sum_cost."
parsePathFileDraw = drawOption (\s o -> o { drawPathFile = Just s }) docPathFile

docOutFile :: OP.Mod
docOutFile = OP.long "outFile"
            OP.<> OP.short "o"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help "File path to where the output file should be written."
parseOutFileLCP  = lcpOption (\s o -> o { lcpOutFile = s }) docOutFile
parseOutFileBFS  = bfsOption (\s o -> o { bfsOutFile = s }) docOutFile
parseOutFileLRW  = lrwOption (\s o -> o { lrwOutFile = s }) docOutFile
parseOutFileDraw = drawOption (\s o -> o { drawOutFile = s }) docOutFile

docNrEdges :: OP.Mod
docNrEdges = OP.long "nrEdges"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "Number of edges that should be walked for each random walk. Default: 20."
parseNrEdgesLRW = lrwOption (\s o -> o { lrwNrEdges = read s }) docNrEdges

docNrPaths :: OP.Mod
docNrPaths = OP.long "nrPaths"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "Number of paths that should be computed for each connection. Default: 1."
parseNrPathsLCP = lcpOption (\s o -> o { lcpNrPaths = read s }) docNrPaths
parseNrPathsLRW = lrwOption (\s o -> o { lrwNrPaths = read s }) docNrPaths

docSeed :: OP.Mod
docSeed = OP.long "seed"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "Seed for random number generation. Default: Nothing."
parseSeedLCP = lcpOption (\s o -> o { lcpSeed = Just (read s) }) docSeed
parseSeedLRW = lrwOption (\s o -> o { lrwSeed = Just (read s) }) docSeed

docDestFile :: OP.Mod
docDestFile = OP.long "destFile"
            OP.<> OP.short "d"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each focal/destination vertex, columns: id."
parseDestFileLCP = lcpOption (\s o -> o { lcpDestFile = Just s }) docDestFile
parseDestFileBFS = bfsOption (\s o -> o { bfsDestFile = s }) docDestFile

docConnectionFile :: OP.Mod
docConnectionFile = OP.long "connectionFile"
            OP.<> OP.short "c"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each pair of vertices, columns: v1, v2, sum_cost."
parseConnectionFile = lcpOption (\s o -> o { lcpConnectionFile = s }) docConnectionFile

docOmissionStrategy :: OP.Mod
docOmissionStrategy = OP.long "omissionStrategy"
            OP.<> OP.metavar "none|omit|filter"
            OP.<> OP.help "Strategy how to handle dests in --destFile. Default: none."
parseOmissionStrategy = lcpOption (\s o -> o { lcpOmissionStrategy = readOmissionStrategy s }) docOmissionStrategy
readOmissionStrategy :: String -> OmissionStrategy
readOmissionStrategy "none" = OmitNone
readOmissionStrategy "omit" = OmitDests
readOmissionStrategy "filter" = FilterInHindsight

docNrMinDests :: OP.Mod
docNrMinDests = OP.long "minDests"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "Minimum number of destinations above which, when found, the search for a \
                          \focal point ceases."
parseNrMinDests = bfsOption (\s o -> o { bfsNrMinDests = read s }) docNrMinDests

docIncDestsByLayer :: OP.Mod
docIncDestsByLayer = OP.long "incDests"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "For each layer opened, increase the number of destinations by adding this \
                          \value. This allows for more connections for isolated destinations."
parseIncDestsByLayer = bfsOption (\s o -> o { bfsIncDestsByLayer = read s }) docIncDestsByLayer
  
docStopAtDests :: OP.Mod
docStopAtDests = OP.long "stopAtDests"
            OP.<> OP.help "Should the search wave stop at a discovered destination?"
parseStopAtDests = bfsFlag (\o -> o { bfsStopAtDests = True }) docStopAtDests

-- combining parsers

cmdParser = OP.optParser $
    OP.commands (OP.metavar "COMMAND") (
        OP.command "lcp" (OP.help "Least-cost path search between pairs of vertices.")
          (\a -> Right $ a { com = LCP (lcpOpts a) }) (
                parseVertFileLCP
            <.> parseEdgeFileLCP
            <.> parseConnectionFile
            <.> parseOmissionStrategy
            <.> parseDestFileLCP
            <.> parseNrPathsLCP
            <.> parseSeedLCP
            <.> parseOutFileLCP
        ) OP.<|>
        OP.command "bfs" (OP.help "Breadth-first search for the n-nearest neighbors on a graph \
                                  \between a list of destination vertices.")
            (\a -> Right $ a { com = BFS (bfsOpts a) }) (
                parseVertFileBFS
            <.> parseEdgeFileBFS
            <.> parseDestFileBFS
            <.> parseNrMinDests
            <.> parseIncDestsByLayer
            <.> parseStopAtDests
            <.> parseOutFileBFS
        ) OP.<|>
        OP.command "lrw" (OP.help "Random walks from focal vertices.")
            (\a -> Right $ a { com = LRW (lrwOpts a) }) (
                parseVertFileLRW
            <.> parseEdgeFileLRW
            <.> parseFocalVertFileLRW
            <.> parseNrEdgesLRW
            <.> parseNrPathsLRW
            <.> parseSeedLRW
            <.> parseOutFileLRW
        ) OP.<|>
        OP.command "draw" (OP.help "Render graphs in GraphViz's DOT language.")
            (\a -> Right $ a { com = Draw (drawOpts a) }) (
                parseVertFileDraw
            <.> parseEdgeFileDraw
            <.> parsePathFileDraw
            <.> parseOutFileDraw
        )
    )

-- default settings
lcpOpts :: Options -> LCPOptions
lcpOpts s = case com s of
  LCP opts -> opts
  _        -> LCPOptions "" "" "" OmitNone Nothing 1 Nothing ""
bfsOpts :: Options -> BFSOptions
bfsOpts s = case com s of
  BFS opts -> opts
  _        -> BFSOptions "" "" "" 6 0 False ""
lrwOpts :: Options -> LRWOptions
lrwOpts s = case com s of
  LRW opts -> opts
  _        -> LRWOptions "" "" "" 20 1 Nothing ""
drawOpts :: Options -> DrawOptions
drawOpts s = case com s of
  Draw opts -> opts
  _         -> DrawOptions "" "" Nothing ""







