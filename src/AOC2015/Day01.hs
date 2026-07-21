module AOC2015.Day01 (run) where

import Control.Applicative (Alternative (empty))
import Control.DeepSeq (force)
import Control.Exception (evaluate, tryJust)
import Control.Monad (guard, (<=<))
import Data.List (foldl')
import System.IO.Error (isDoesNotExistError)

--------------------------------------------------------------------------------

eitherToMaybe :: (Alternative m) => Either e a -> m a
eitherToMaybe = either (const empty) pure

stripNewline :: String -> String
stripNewline = reverse . dropWhile (== '\n') . reverse

--------------------------------------------------------------------------------

inputFilePath :: FilePath
inputFilePath = "data/2015/01.txt"

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe = (traverse (evaluate . force) . eitherToMaybe) <=< tryJust (guard . isDoesNotExistError) . readFile

--------------------------------------------------------------------------------

level :: String -> Integer
level = foldl' go 0
  where
    go i c
      | c == '(' = i + 1
      | c == ')' = i - 1
      | otherwise = undefined

run :: IO ()
run = do
  input <- readFileMaybe inputFilePath
  case input of
    Nothing -> putStrLn "cannot read input"
    Just (stripNewline -> inp) -> do
      putStrLn inp
      print $ level inp
