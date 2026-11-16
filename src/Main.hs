module Main (main) where

import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main = do
  day <- getArgs >>= parse
  putStrLn ("Hello world: " ++ show day)

parse :: [String] -> IO Integer
parse [] = fail "no arguments given"
parse (s : _) = case readMaybe s of
  Nothing -> fail $ "not an integer: " ++ s
  Just i -> return i
