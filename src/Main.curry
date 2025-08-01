module Main where

import LCP

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

defaultOptions :: Options
defaultOptions = Options NoCommand

lcpOpts :: Options -> LCPOptions
lcpOpts s = case optCommand s of
  LCP opts -> opts
  _        -> LCPOptions "" "" "" ""

applyEither :: [Options -> Either String Options] -> Options
            -> Either String Options
applyEither [] z = Right z
applyEither (f:fs) z = case f z of
  Left err -> Left err
  Right z' -> applyEither fs z'

applyParse :: [Options -> Either String Options] -> Either String Options
applyParse fs = applyEither fs defaultOptions

cmdParser = OP.optParser $
    OP.commands (OP.metavar "COMMAND") $
        OP.command "lcp" (OP.help "...")
                (\a -> Right $ a { optCommand = LCP (lcpOpts a) })
            $
            OP.option (\s a -> Right $ a { optCommand = LCP (lcpOpts a) { vertFile = s } }) (
                OP.long "vertFile"
                OP.<> OP.short "v"
                OP.<> OP.metavar "PATH"
                OP.<> OP.help "..."
            ) OP.<.> OP.option (\s a -> Right $ a { optCommand = LCP (lcpOpts a) { edgeFile = s } }) (
                OP.long "edgeFiles"
                OP.<> OP.short "e"
                OP.<> OP.metavar "PATH"
                OP.<> OP.help "..."
            ) OP.<.> OP.option (\s a -> Right $ a { optCommand = LCP (lcpOpts a) { connectionFile = s } }) (
                OP.long "connectionFile"
                OP.<> OP.short "c"
                OP.<> OP.metavar "PATH"
                OP.<> OP.help "..."
            ) OP.<.> OP.option (\s a -> Right $ a { optCommand = LCP (lcpOpts a) { outFile = s } }) (
                OP.long "outFile"
                OP.<> OP.short "o"
                OP.<> OP.metavar "PATH"
                OP.<> OP.help "..."
            )

main :: IO ()
main = do
  args <- getArgs
  parseResult <- return $ OP.parse (intercalate " " args) cmdParser "cnn"
  case parseResult of
    Left err -> putStrLn err
    Right  r -> 
        case applyParse r of
              Left err   -> do putStrLn err
              Right opts -> runWithArgs opts
    
runWithArgs :: Options -> IO ()
runWithArgs (Options (LCP opts)) = runLCP opts




















