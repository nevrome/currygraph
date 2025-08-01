module Main where

import Types
import LCP

import Data.List
import Data.Maybe
import qualified OptParse as OP
import System.Environment
import Data.Global
import System.IO
import System.IO.Unsafe

cmdParser = OP.optParser $
    OP.option (\s a -> a { vertFile = s }) (
        OP.long "vertFile"
        OP.<> OP.short "v"
        OP.<> OP.metavar "PATH"
        OP.<> OP.help "..."
    ) OP.<.> OP.option (\s a -> a { edgeFile = s }) (
        OP.long "edgeFiles"
        OP.<> OP.short "e"
        OP.<> OP.metavar "PATH"
        OP.<> OP.help "..."
    ) OP.<.> OP.option (\s a -> a { connectionFile = s }) (
        OP.long "connectionFile"
        OP.<> OP.short "c"
        OP.<> OP.metavar "PATH"
        OP.<> OP.help "..."
    ) OP.<.> OP.option (\s a -> a { outFile = s }) (
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
    Right  v -> do
        let options = applyParse v
        runCNN options





















