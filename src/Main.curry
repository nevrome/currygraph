module Main where

import LCP
import BFS

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

docEdgeFile :: OP.Mod
docEdgeFile = OP.long "edgeFile"
            OP.<> OP.short "e"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each edge, columns: v1, v2, cost."
parseEdgeFileLCP =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpEdgeFile = s}}) docEdgeFile
parseEdgeFileBFS =
    OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsEdgeFile = s}}) docEdgeFile

docOutFile :: OP.Mod
docOutFile = OP.long "outFile"
            OP.<> OP.short "o"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help "File path to where the output .csv file should be written."
parseOutFileLCP =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpOutFile = s}}) docOutFile
parseOutFileBFS =
    OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsOutFile = s}}) docOutFile

docVerbose :: OP.Mod
docVerbose = OP.long "verbose"
             OP.<> OP.short "v"
             OP.<> OP.help "Should the CLI output be more informative? Default: False."
--parseVerboseLCP =
--    OP.flag (\a -> Right $ a {com = LCP (lcpOpts a) {lcpVerbose = True}}) docVerbose
--parseOutFileBFS =
--    OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsOutFile = s}}) docOutFile

-- only lcp

docConnectionFile :: OP.Mod
docConnectionFile = OP.long "connectionFile"
            OP.<> OP.short "c"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each pair of vertices, columns: v1, v2, sum_cost."
parseConnectionFile =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpConnectionFile = s}}) docConnectionFile

docNrPaths :: OP.Mod
docNrPaths = OP.long "nrPaths"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "Number of paths that should be computed for each connection. Default: 1."
parseNrPaths =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpNrPaths = read s}}) docNrPaths

docSeed :: OP.Mod
docSeed = OP.long "seed"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "Seed for random number gerneration. Default: Nothing."
parseSeed =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpSeed = Just $ read s}}) docSeed

-- only bfs

docDestFile :: OP.Mod
docDestFile = OP.long "destFile"
            OP.<> OP.short "d"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each focal/destination vertex, columns: id, long, lat."
parseDestFile =
    OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsDestFile = s}}) docDestFile

docMinDests :: OP.Mod
docMinDests = OP.long "minDests"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "Minimum number of destinations above which, when found, the search for a \
                          \focal point ceases."
parseMinDests =
    OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsMinNrDestinations = read s}}) docMinDests

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
            <.> parseNrPaths
            <.> parseSeed
            <.> parseOutFileLCP
        ) OP.<|>
        OP.command "bfs" (OP.help "Breadth-first search for the n-nearest neighbors on a graph \
                                  \between a list of destination vertices.")
            (\a -> Right $ a { com = BFS (bfsOpts a) }) (
                parseVertFileBFS
            <.> parseEdgeFileBFS
            <.> parseDestFile
            <.> parseMinDests
            <.> parseStopAtDests
            <.> parseOutFileBFS
        )
    )

-- default settings
lcpOpts :: Options -> LCPOptions
lcpOpts s = case com s of
  LCP opts -> opts
  _        -> LCPOptions "" "" "" 1 Nothing ""
bfsOpts :: Options -> BFSOptions
bfsOpts s = case com s of
  BFS opts -> opts
  _        -> BFSOptions "" "" "" 6 False ""








