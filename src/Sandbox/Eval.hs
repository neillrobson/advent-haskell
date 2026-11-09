{-# LANGUAGE ScopedTypeVariables #-}

module Sandbox.Eval () where

import Control.DeepSeq (force)
import Control.Exception (ArithException, evaluate, try)

xs :: [Int]
xs = [1, 2, 1 `div` 0]

probe :: forall a. String -> IO a -> IO ()
probe label act = do
  r <- try act :: IO (Either ArithException a)
  putStrLn (label ++ ": " ++ either (const "error") (const "success") r)

main :: IO ()
main = do
  probe "return xs" $ return xs
  probe "evaluate xs" $ evaluate xs
  probe "evaluate (length xs)" $ evaluate (length xs)
  probe "evaluate (sum xs)" $ evaluate (sum xs)
  probe "evaluate (force xs)" $ evaluate (force xs)
  probe "return (force xs)" $ return (force xs)
