module Sandbox.Lock (main) where

import Control.DeepSeq
import Control.Exception

main :: IO ()
main = do
  writeFile "f.txt" "original"
  s <- readFile "f.txt"
  r <- try (writeFile "f.txt" "changed") :: IO (Either IOException ())
  print r
  putStrLn s

  -- Neither evaluate nor force alone would resolve the exception
  writeFile "f.txt" "original"
  s2 <- (evaluate . force) =<< readFile "f.txt"
  writeFile "f.txt" "changed"
  putStrLn s2
