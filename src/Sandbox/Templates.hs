module Sandbox.Templates (bigBadMathProblem, precompute) where

import Language.Haskell.TH

-- Source: https://www.parsonsmatt.org/2015/11/15/template_haskell.html

bigBadMathProblem :: Int -> Double
bigBadMathProblem = (2 /) . fromIntegral

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
  let patterns = map intToPat xs
  let bodies = map precomputeInteger xs
  let fallbackArgName = mkName "x"
  let bbmpName = mkName "bigBadMathProblem"
  let fallbackBodyExp = AppE (VarE bbmpName) (VarE fallbackArgName)
  let lastClause = Clause [VarP fallbackArgName] (NormalB fallbackBodyExp) []
  let precomputedClauses = zipWith (\pat body -> Clause [pat] (NormalB body) []) patterns bodies
  let clauses = precomputedClauses ++ [lastClause]
  return [FunD name clauses]
