module Sandbox.Templates (precompute, pretwo, primesUpTo') where

import Debug.Trace (trace)
import Language.Haskell.TH
import Sandbox.Primes (isPrime)

-- Source: https://www.parsonsmatt.org/2015/11/15/template_haskell.html

bigBadMathProblem :: Int -> Double
bigBadMathProblem = trace "calling bigBadMathProblem" . (2 /) . fromIntegral

{-
Desired output of precompute:

lookupTable :: Int -> Double
lookupTable 0 = 123.456
lookupTable 12 = 151626.4234
...
lookupTable x = bigBadMathProblem x

Each Int in our input list should be one pattern in the output.
-}

intToPat :: Int -> Pat
intToPat = LitP . IntegerL . toInteger

precomputeInteger :: Int -> Exp
precomputeInteger = LitE . RationalL . toRational . bigBadMathProblem

precompute :: [Int] -> DecsQ
precompute xs = do
  fallbackArgName <- newName "x"
  lastClause <-
    let fallbackBodyExp = [e|bigBadMathProblem $(varE fallbackArgName)|]
     in clause [varP fallbackArgName] (normalB fallbackBodyExp) []
  let name = mkName "lookupTable"
      patterns = map intToPat xs
      bodies = map precomputeInteger xs
      precomputedClauses = zipWith (\pat body -> Clause [pat] (NormalB body) []) patterns bodies
      clauses = precomputedClauses ++ [lastClause]
  signature <- sigD name [t|Int -> Double|]
  return [signature, FunD name clauses]

intToQPat :: Int -> PatQ
intToQPat = litP . integerL . toInteger

intToQExp :: Int -> ExpQ
intToQExp = litE . rationalL . toRational . bigBadMathProblem

pretwo :: [Int] -> DecsQ
pretwo xs = sequence [signature, funD name clauses]
  where
    name = mkName "lookupTableTwo"
    signature = sigD name [t|Int -> Double|]
    patterns = map intToQPat xs
    bodies = map intToQExp xs
    precomputedClauses = zipWith (\pat body -> clause [pat] (normalB body) []) patterns bodies
    lastClause = do
      arg <- newName "x"
      let fallbackBodyExp = [e|bigBadMathProblem $(varE arg)|]
      clause [varP arg] (normalB fallbackBodyExp) []
    clauses = precomputedClauses ++ [lastClause]

--------------------------------------------------------------------------------

primesUpTo' :: Integer -> Code Q [Integer]
primesUpTo' n = go 2
  where
    go i
      | i > n = [||[]||]
      | isPrime i = [||i : $$(go $ i + 1)||]
      | otherwise = go $ i + 1
