module Main (main) where

import AOC2015.Day01 (day01a, day01b)
import AOC2015.Day02 (day02a, day02b)
import Core (SomeSolution (MkSomeSol), runSome)
import Data.Map (Map)
import qualified Data.Map as M
import System.Environment (getArgs)
import Text.Read (readMaybe)

newtype Day = Day {dayInt :: Integer} deriving (Eq, Ord)

data Part = Part1 | Part2 deriving (Eq, Ord)

type ChallengeMap = Map Day (Map Part SomeSolution)

files :: Day -> FilePath
files (dayInt -> 1) = "data/2015/01.txt"
files (dayInt -> 2) = "data/2015/02.txt"
files _ = undefined

challenges :: ChallengeMap
challenges =
  M.fromList
    [ ( Day 1,
        M.fromList
          [ (Part1, MkSomeSol day01a),
            (Part2, MkSomeSol day01b)
          ]
      ),
      ( Day 2,
        M.fromList
          [ (Part1, MkSomeSol day02a),
            (Part2, MkSomeSol day02b)
          ]
      )
    ]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  (day, part) <- case parse args of
    Left err -> fail err
    Right dp -> return dp
  runChallenge challenges day part

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

runChallenge :: ChallengeMap -> Day -> Part -> IO ()
runChallenge cm day part = do
  someSol <- case M.lookup day cm >>= M.lookup part of
    Just ss -> pure ss
    Nothing -> fail "not yet implemented"
  runSome (files day) someSol
