{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Sandbox.Splices (lookupTable, lookupTableTwo) where

import Sandbox.Templates (precompute, pretwo)

$(precompute [1 .. 10])

$(pretwo [1 .. 10])
