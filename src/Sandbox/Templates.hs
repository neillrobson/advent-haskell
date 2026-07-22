module Sandbox.Templates (bigBadMathProblem, precompute) where

import Debug.Trace (trace)
import Language.Haskell.TH

-- Source: https://www.parsonsmatt.org/2015/11/15/template_haskell.html

bigBadMathProblem :: Int -> Double
bigBadMathProblem = trace "calling bigBadMathProblem" . (2 /) . fromIntegral

{-
Desired output of precompute:

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
  let name = mkName "lookupTable"
      patterns = map intToPat xs
      bodies = map precomputeInteger xs
      fallbackArgName = mkName "x"
      bbmpName = mkName "bigBadMathProblem"
      fallbackBodyExp = AppE (VarE bbmpName) (VarE fallbackArgName)
      lastClause = Clause [VarP fallbackArgName] (NormalB fallbackBodyExp) []
      precomputedClauses = zipWith (\pat body -> Clause [pat] (NormalB body) []) patterns bodies
      clauses = precomputedClauses ++ [lastClause]
  return [FunD name clauses]
