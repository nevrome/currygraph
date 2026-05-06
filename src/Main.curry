module Main where

import LCP
import BFS
import LRW

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
runWithArgs (Options (LCP opts)) = runLCP opts
runWithArgs (Options (BFS opts)) = runBFS opts
runWithArgs (Options (LRW opts)) = runLRW opts

-- parsers

-- general
docVertFile :: OP.Mod
docVertFile = OP.long "vertFile"
            OP.<> OP.short "v"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each vertex, columns: id, long, lat."
parseVertFileLCP =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpVertFile = s}}) docVertFile
parseVertFileBFS =
    OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsVertFile = s}}) docVertFile
parseVertFileLRW =
    OP.option (\s a -> Right $ a {com = LRW (lrwOpts a) {lrwVertFile = s}}) docVertFile

docFocalVertFile :: OP.Mod
docFocalVertFile = OP.long "focalVertFile"
            OP.<> OP.short "f"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each vertex, columns: id, long, lat."
parseFocalVertFileLRW =
    OP.option (\s a -> Right $ a {com = LRW (lrwOpts a) {lrwFocalVertFile = s}}) docFocalVertFile

docEdgeFile :: OP.Mod
docEdgeFile = OP.long "edgeFile"
            OP.<> OP.short "e"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each edge, columns: v1, v2, cost."
parseEdgeFileLCP =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpEdgeFile = s}}) docEdgeFile
parseEdgeFileBFS =
    OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsEdgeFile = s}}) docEdgeFile
parseEdgeFileLRW =
    OP.option (\s a -> Right $ a {com = LRW (lrwOpts a) {lrwEdgeFile = s}}) docEdgeFile

docOutFile :: OP.Mod
docOutFile = OP.long "outFile"
            OP.<> OP.short "o"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help "File path to where the output .csv file should be written."
parseOutFileLCP =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpOutFile = s}}) docOutFile
parseOutFileBFS =
    OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsOutFile = s}}) docOutFile
parseOutFileLRW =
    OP.option (\s a -> Right $ a {com = LRW (lrwOpts a) {lrwOutFile = s}}) docOutFile

docNrEdges :: OP.Mod
docNrEdges = OP.long "nrEdges"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "Number of edges that should be walked for each random walk. Default: 20."
parseNrEdgesLRW =
    OP.option (\s a -> Right $ a {com = LRW (lrwOpts a) {lrwNrEdges = read s}}) docNrEdges

docNrPaths :: OP.Mod
docNrPaths = OP.long "nrPaths"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "Number of paths that should be computed for each connection. Default: 1."
parseNrPathsLCP =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpNrPaths = read s}}) docNrPaths
parseNrPathsLRW =
    OP.option (\s a -> Right $ a {com = LRW (lrwOpts a) {lrwNrPaths = read s}}) docNrPaths

docSeed :: OP.Mod
docSeed = OP.long "seed"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "Seed for random number gerneration. Default: Nothing."
parseSeedLCP =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpSeed = Just $ read s}}) docSeed
parseSeedLRW =
    OP.option (\s a -> Right $ a {com = LRW (lrwOpts a) {lrwSeed = Just $ read s}}) docSeed

docVerbose :: OP.Mod
docVerbose = OP.long "verbose"
             OP.<> OP.short "v"
             OP.<> OP.help "Should the CLI output be more informative? Default: False."

docDestFile :: OP.Mod
docDestFile = OP.long "destFile"
            OP.<> OP.short "d"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each focal/destination vertex, columns: id."
parseDestFileLCP =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpDestFile = Just s}}) docDestFile
parseDestFileBFS =
    OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsDestFile = s}}) docDestFile

docConnectionFile :: OP.Mod
docConnectionFile = OP.long "connectionFile"
            OP.<> OP.short "c"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each pair of vertices, columns: v1, v2, sum_cost."
parseConnectionFile =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpConnectionFile = s}}) docConnectionFile

docOmissionStrategy :: OP.Mod
docOmissionStrategy = OP.long "omissionStrategy"
            OP.<> OP.metavar "none|omit|filter"
            OP.<> OP.help "Strategy how to handle dests in --destFile. Default: none."
parseOmissionStrategy =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpOmissionStrategy = readOmissionStrategy s}}) docOmissionStrategy
readOmissionStrategy :: String -> OmissionStrategy
readOmissionStrategy "none" = OmitNone
readOmissionStrategy "omit" = OmitDests
readOmissionStrategy "filter" = FilterInHindsight

docNrMinDests :: OP.Mod
docNrMinDests = OP.long "minDests"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "Minimum number of destinations above which, when found, the search for a \
                          \focal point ceases."
parseNrMinDests =
    OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsNrMinDests = read s}}) docNrMinDests

docIncDestsByLayer :: OP.Mod
docIncDestsByLayer = OP.long "incDests"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "For each layer opened, increase the number of destinations by adding this value. \
                          \This allows for more connections for isolated destinations."
parseIncDestsByLayer =
    OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsIncDestsByLayer = read s}}) docIncDestsByLayer

docStopAtDests :: OP.Mod
docStopAtDests = OP.long "stopAtDests"
            OP.<> OP.help "Should the search wave stop at a discovered destination?"
parseStopAtDests =
    OP.flag (\a -> Right $ a {com = BFS (bfsOpts a) {bfsStopAtDests = True}}) docStopAtDests

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







