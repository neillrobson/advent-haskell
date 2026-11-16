module Main (main) where

import AOC2015.Day01 (partA, partB, run)
import System.Environment (getArgs)
import Text.Read (readMaybe)

newtype Day = Day {dayInt :: Integer}

data Part = Part1 | Part2

main :: IO ()
main = do
  day <- getArgs >>= parse
  putStrLn ("Hello world: " ++ show day)

parse :: [String] -> IO Integer
parse [] = fail "no arguments given"
parse [_] = fail "not enough arguments given"
parse (s : _) = case readMaybe s of
  Nothing -> fail $ "not an integer: " ++ s
  Just i -> return i

runDay :: Day -> Part -> IO ()
runDay (dayInt -> 1) Part1 = run partA
runDay (dayInt -> 1) Part2 = run partB
runDay _ _ = fail "not yet implemented"
