module AOC2015.Day01 (day01a, day01b) where

import Core ((:~>) (MkSol))
import Data.List (foldl')
import Util (stripNewline)

day01a :: String :~> Integer
day01a = MkSol (Just . stripNewline) (Just . partA) show

day01b :: String :~> Integer
day01b = MkSol (Just . stripNewline) (Just . partB) show

--------------------------------------------------------------------------------

partA :: String -> Integer
partA = foldl' go 0
  where
    go i c
      | c == '(' = i + 1
      | c == ')' = i - 1
      | otherwise = undefined

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
