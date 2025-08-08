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

-- only lcp

docConnectionFile :: OP.Mod
docConnectionFile = OP.long "connectionFile"
            OP.<> OP.short "c"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help ".csv file. One row for each pair of vertices, columns: v1, v2, sum_cost."
parseConnectionFile =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpConnectionFile = s}}) docConnectionFile

docDeleteUsedEdges :: OP.Mod
docDeleteUsedEdges = OP.long "deleteUsedEdges"
            OP.<> OP.help "Should edges used by preceding connections be reused? \
                          \Note that the order of connections in --connectionFile matters with this. \
                          \Default: False."
parseDeleteUsedEdges =
    OP.flag (\a -> Right $ a {com = LCP (lcpOpts a) {lcpDeleteUsedEdges = True}}) docDeleteUsedEdges

docMaxNrBranches :: OP.Mod
docMaxNrBranches = OP.long "maxNrBranches"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "Maximum number of paths that should be tried for one connection. \
                          \Default: 1000."
parseMaxNrBranches =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpMaxNrBranches = read s}}) docMaxNrBranches

docCostThresholdAbs :: OP.Mod
docCostThresholdAbs = OP.long "absCostThreshold"
            OP.<> OP.metavar "FLOAT"
            OP.<> OP.help "Maximum cost threshold above which a path should be pruned. \
                          \Set either this, --relCostThreshold, or nothing. Default: None."
parseCostThresholdAbs =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpCostThreshold = Absolute $ read s}}) docCostThresholdAbs

docCostThresholdRel :: OP.Mod
docCostThresholdRel = OP.long "relCostThreshold"
            OP.<> OP.metavar "FLOAT"
            OP.<> OP.help "Maximum cost threshold just as --absCostThreshold, but as a multiplication \
                          \factor applied to the sum_cost of the given connection.\
                          \Set either this, --absCostThreshold, or nothing. Default: None."
parseCostThresholdRel =
    OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpCostThreshold = Relative $ read s}}) docCostThresholdRel

docUpdateCostThreshold :: OP.Mod
docUpdateCostThreshold = OP.long "updateCostThreshold"
            OP.<> OP.help "Should the cost threshold for path pruning be updated as soon as a path \
                          \is discovered. From that point onward --absCostThreshold and --relCostThreshold \
                          \are obsolete. Default: False."
parseUpdateCostThreshold =
    OP.flag (\a -> Right $ a {com = LCP (lcpOpts a) {lcpUpdateCostThreshold = True}}) docUpdateCostThreshold

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
        OP.command "lcp" (OP.help "Depth-first least-cost path search on a spatial graph between \
                                  \pairs of vertices.")
          (\a -> Right $ a { com = LCP (lcpOpts a) }) (
                parseVertFileLCP
            <.> parseEdgeFileLCP
            <.> parseConnectionFile
            <.> parseDeleteUsedEdges
            <.> parseMaxNrBranches
            <.> (parseCostThresholdAbs <|> parseCostThresholdRel)
            <.> parseUpdateCostThreshold
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
  _        -> LCPOptions "" "" "" False 1000 None False ""
bfsOpts :: Options -> BFSOptions
bfsOpts s = case com s of
  BFS opts -> opts
  _        -> BFSOptions "" "" "" 6 False ""








