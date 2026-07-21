module Sandbox.Templates () where

import Language.Haskell.TH

-- Source: https://www.parsonsmatt.org/2015/11/15/template_haskell.html

precompute :: [Int] -> DecsQ
precompute xs = do
  -- fill me in
  let name = undefined
  let clauses = undefined
  return [FunD name clauses]
