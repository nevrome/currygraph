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

-- general

docVertFile :: OP.Mod
docVertFile = OP.long "vertFile"
            OP.<> OP.short "v"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help "..."

docEdgeFile :: OP.Mod
docEdgeFile = OP.long "edgeFile"
            OP.<> OP.short "e"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help "..."

docOutFile :: OP.Mod
docOutFile = OP.long "outFile"
            OP.<> OP.short "o"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help "..."

-- only lcp

docConnectionFile :: OP.Mod
docConnectionFile = OP.long "connectionFile"
            OP.<> OP.short "c"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help "..."

lcpOpts :: Options -> LCPOptions
lcpOpts s = case com s of
  LCP opts -> opts
  _        -> LCPOptions "" "" "" ""

-- only bfs

docDestFile :: OP.Mod
docDestFile = OP.long "destFile"
            OP.<> OP.short "d"
            OP.<> OP.metavar "PATH"
            OP.<> OP.help "..."

docMinDests :: OP.Mod
docMinDests = OP.long "minDests"
            OP.<> OP.metavar "INT"
            OP.<> OP.help "..."

docStopAtDests :: OP.Mod
docStopAtDests = OP.long "stopAtDests"
            OP.<> OP.metavar "Bool"
            OP.<> OP.help "..."

bfsOpts :: Options -> BFSOptions
bfsOpts s = case com s of
  BFS opts -> opts
  _        -> BFSOptions "" "" "" 6 False ""

cmdParser = OP.optParser $
    OP.commands (OP.metavar "COMMAND") (
        OP.command "lcp" (OP.help "...")
          (\a -> Right $ a { com = LCP (lcpOpts a) }) (
                OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpVertFile = s}}) docVertFile
            <.> OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpEdgeFile = s}}) docEdgeFile
            <.> OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpConnectionFile = s}}) docConnectionFile
            <.> OP.option (\s a -> Right $ a {com = LCP (lcpOpts a) {lcpOutFile = s}}) docOutFile
        ) OP.<|>
        OP.command "bfs" (OP.help "...")
            (\a -> Right $ a { com = BFS (bfsOpts a) }) (
                OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsVertFile = s}}) docVertFile
            <.> OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsEdgeFile = s}}) docEdgeFile
            <.> OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsDestFile = s}}) docDestFile
            <.> OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsMinNrDestinations = read s}}) docMinDests
            <.> OP.flag (\a -> Right $ a {com = BFS (bfsOpts a) {bfsStopAtDests = True}}) docStopAtDests
            <.> OP.option (\s a -> Right $ a {com = BFS (bfsOpts a) {bfsOutFile = s}}) docOutFile
        )
    )

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




















