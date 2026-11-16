module Main (main) where

import AOC2015.Day01 (partA, partB, run)
import System.Environment (getArgs)
import Text.Read (readMaybe)

newtype Day = Day {dayInt :: Integer}

data Part = Part1 | Part2

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
runDay (dayInt -> 1) Part1 = run partA
runDay (dayInt -> 1) Part2 = run partB
runDay _ _ = fail "not yet implemented"
