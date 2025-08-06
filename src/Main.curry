module Main where

import LCP
import BFS

import Data.List
import Data.Maybe
import qualified OptParse as OP
import System.Environment
import Data.Global
import System.IO
import System.IO.Unsafe

data Options = Options { optCommand :: Command }
data Command =
      NoCommand
    | LCP LCPOptions
    | BFS BFSOptions

defaultOptions :: Options
defaultOptions = Options NoCommand

lcpOpts :: Options -> LCPOptions
lcpOpts s = case optCommand s of
  LCP opts -> opts
  _        -> LCPOptions "" "" "" ""
bfsOpts :: Options -> BFSOptions
bfsOpts s = case optCommand s of
  BFS opts -> opts
  _        -> BFSOptions "" "" 6 ""

applyEither :: [Options -> Either String Options] -> Options -> Either String Options
applyEither [] z = Right z
applyEither (f:fs) z = case f z of
  Left err -> Left err
  Right z' -> applyEither fs z'
applyParse :: [Options -> Either String Options] -> Either String Options
applyParse fs = applyEither fs defaultOptions

cmdParser = OP.optParser $
    OP.commands (OP.metavar "COMMAND") (
        OP.command "lcp" (OP.help "...") (\a -> Right $ a { optCommand = LCP (lcpOpts a) }) (            
            OP.option (\s a -> Right $ a { optCommand = LCP (lcpOpts a) { lcpVertFile = s } }) (
                OP.long "vertFile"
                OP.<> OP.short "v"
                OP.<> OP.metavar "PATH"
                OP.<> OP.help "..."
            ) OP.<.> OP.option (\s a -> Right $ a { optCommand = LCP (lcpOpts a) { lcpEdgeFile = s } }) (
                OP.long "edgeFiles"
                OP.<> OP.short "e"
                OP.<> OP.metavar "PATH"
                OP.<> OP.help "..."
            ) OP.<.> OP.option (\s a -> Right $ a { optCommand = LCP (lcpOpts a) { lcpConnectionFile = s } }) (
                OP.long "connectionFile"
                OP.<> OP.short "c"
                OP.<> OP.metavar "PATH"
                OP.<> OP.help "..."
            ) OP.<.> OP.option (\s a -> Right $ a { optCommand = LCP (lcpOpts a) { lcpOutFile = s } }) (
                OP.long "outFile"
                OP.<> OP.short "o"
                OP.<> OP.metavar "PATH"
                OP.<> OP.help "..."
            )
        ) OP.<|> (
        OP.command "bfs" (OP.help "...")
                (\a -> Right $ a { optCommand = BFS (bfsOpts a) })
            $
            OP.option (\s a -> Right $ a { optCommand = BFS (bfsOpts a) { bfsVertFile = s } }) (
                OP.long "vertFile"
                OP.<> OP.short "v"
                OP.<> OP.metavar "PATH"
                OP.<> OP.help "..."
            ) OP.<.> OP.option (\s a -> Right $ a { optCommand = BFS (bfsOpts a) { bfsEdgeFile = s } }) (
                OP.long "edgeFiles"
                OP.<> OP.short "e"
                OP.<> OP.metavar "PATH"
                OP.<> OP.help "..."
             ) OP.<.> OP.option (\s a -> Right $ a { optCommand = BFS (bfsOpts a) { bfsMinNrDestinations = read s } }) (
                OP.long "minDests"
                OP.<> OP.metavar "INT"
                OP.<> OP.help "..."
            ) OP.<.> OP.option (\s a -> Right $ a { optCommand = BFS (bfsOpts a) { bfsOutFile = s } }) (
                OP.long "outFile"
                OP.<> OP.short "o"
                OP.<> OP.metavar "PATH"
                OP.<> OP.help "..."
            )
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




















