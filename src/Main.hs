module Main (main) where

import AOC2015.Day01 (day01a, day01b)
import AOC2015.Day02 (day02a)
import Core (run)
import System.Environment (getArgs)
import Text.Read (readMaybe)

newtype Day = Day {dayInt :: Integer}

data Part = Part1 | Part2

files :: Day -> FilePath
files (dayInt -> 1) = "data/2015/01.txt"
files (dayInt -> 2) = "data/2015/02.txt"
files _ = undefined

main :: IO ()
main = do
  args <- getArgs
  (day, part) <- case parse args of
    Left err -> fail err
    Right dp -> return dp
  runDay day part

parse :: [String] -> Either String (Day, Part)
parse [] = Left "no arguments given"
parse [_] = Left "not enough arguments given"
parse (d : p : _) = do
  di <- maybe (Left $ "cannot parse day: " ++ d) Right $ readMaybe d
  part <- case p of
    "a" -> Right Part1
    "b" -> Right Part2
    _ -> Left $ "invalid part: " ++ p
  return (Day di, part)

runDay :: Day -> Part -> IO ()
runDay (dayInt -> 1) Part1 = run (files $ Day 1) day01a
runDay (dayInt -> 1) Part2 = run (files $ Day 1) day01b
runDay (dayInt -> 2) Part1 = run (files $ Day 2) day02a
runDay _ _ = fail "not yet implemented"
