module Main where

import Data.List
import Data.Maybe
import qualified OptParse as OP
import System.Environment
import Control.Search.AllValues
import Text.CSV (readCSVFile)

data Options = Options {
      edgeFile :: String
    , startVertex :: Int
    , endVertex :: Int
} deriving Show

cmdParser = OP.optParser $
    OP.option (\s a -> a { edgeFile = s }) (
        OP.long "edgeFile"
        OP.<> OP.short "e"
        OP.<> OP.metavar "PATH"
        OP.<> OP.help "..."
    ) OP.<.> OP.option (\s a -> a { startVertex = read s }) (
        OP.long "startVertex"
        OP.<> OP.short "a"
        OP.<> OP.metavar "INT"
        OP.<> OP.help "..."
    ) OP.<.> OP.option (\s a -> a { endVertex = read s }) (
        OP.long "endVertex"
        OP.<> OP.short "b"
        OP.<> OP.metavar "INT"
        OP.<> OP.help "..."
    )

applyParse :: [Options -> Options] -> Options
applyParse fs = foldl (flip apply) defaultOpts fs
    where
        defaultOpts = Options "" 0 0

main :: IO ()
main = do
  args <- getArgs
  parseResult <- return $ OP.parse (intercalate " " args) cmdParser "cnn"
  case parseResult of
    Left err -> putStrLn err
    Right  v -> do
        let options = applyParse v
        runCNN options

runCNN :: Options -> IO ()
runCNN (Options edgeFile startVertex endVertex) = do
    putStrLn "huhu"

