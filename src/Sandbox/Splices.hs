{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Sandbox.Splices (lookupTable) where

import Sandbox.Templates (precompute)

$(precompute [1 .. 10])
