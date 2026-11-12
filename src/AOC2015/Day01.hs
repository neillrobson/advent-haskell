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

partA :: String -> Integer
partA = foldl' go 0
  where
    go i c
      | c == '(' = i + 1
      | c == ')' = i - 1
      | otherwise = undefined

run :: (String -> Integer) -> IO ()
run f = do
  input <- readFileMaybe inputFilePath
  case input of
    Nothing -> putStrLn "cannot read input"
    Just (stripNewline -> inp) -> do
      putStrLn inp
      print $ f inp

--------------------------------------------------------------------------------

partB :: String -> Integer
partB = go (0 :: Integer) 0
  where
    go l i []
      | l < 0 = i
      | otherwise = -1
    go l i (c : s)
      | l < 0 = i
      | c == '(' = go (l + 1) (i + 1) s
      | c == ')' = go (l - 1) (i + 1) s
      | otherwise = -1
