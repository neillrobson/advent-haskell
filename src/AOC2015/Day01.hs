module AOC2015.Day01 (run) where

import Core (readFileMaybe)
import Data.List (foldl')

--------------------------------------------------------------------------------

stripNewline :: String -> String
stripNewline = reverse . dropWhile (== '\n') . reverse

--------------------------------------------------------------------------------

inputFilePath :: FilePath
inputFilePath = "data/2015/01.txt"

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
