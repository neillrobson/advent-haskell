module Sandbox.Lock (main) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import System.IO (IOMode (ReadMode), hGetContents, readFile', withFile)

main :: IO ()
main = do
  writeFile "f.txt" "original"
  -- Would fail with regular `readFile`
  s <- readFile' "f.txt"
  r <- try (writeFile "f.txt" "changed") :: IO (Either IOException ())
  print r
  putStrLn s

  -- Neither evaluate nor force alone would resolve the exception
  writeFile "f.txt" "original"
  s2 <- (evaluate . force) =<< readFile "f.txt"
  writeFile "f.txt" "changed"
  putStrLn s2

  s3 <- withFile "f.txt" ReadMode ((evaluate . force) <=< hGetContents)
  putStrLn s3
