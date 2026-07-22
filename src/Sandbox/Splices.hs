{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Sandbox.Splices (lookupTable, lookupTableTwo, firstHundredPrimes) where

import Sandbox.Templates (precompute, pretwo, primesUpTo')

$(precompute [1 .. 10])

$(pretwo [1 .. 10])

firstHundredPrimes :: [Integer]
firstHundredPrimes = $$(primesUpTo' 100)
