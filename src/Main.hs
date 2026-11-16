module Main (main) where

import System.Environment (getArgs)

main :: IO ()
main = do
  day <- getArgs >>= parse
  putStrLn ("Hello world: " ++ show day)

parse :: [String] -> IO Integer
parse [] = fail "no arguments given"
parse (s : _) = readIO s
