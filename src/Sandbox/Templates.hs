module Sandbox.Templates () where

import Language.Haskell.TH

-- Source: https://www.parsonsmatt.org/2015/11/15/template_haskell.html

{-
Desired output of precompute:

lookupTable 0 = 123.456
lookupTable 12 = 151626.4234
...
lookupTable x = bigBadMathProblem x

Each Int in our input list should be one pattern in the output.
-}

precompute :: [Int] -> DecsQ
precompute xs = do
  let name = mkName "lookupTable"
  let clauses = undefined
  return [FunD name clauses]
