module Sandbox.Templates (precompute) where

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
precomputeInteger = LitE . DoublePrimL . toRational . bigBadMathProblem

precompute :: [Int] -> DecsQ
precompute xs = do
  let name = mkName "lookupTable"
  let patterns = map intToPat xs
  let bodies = map precomputeInteger xs
  let clauses = zipWith (\pat body -> Clause [pat] (NormalB body) []) patterns bodies
  return [FunD name clauses]
