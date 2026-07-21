{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Sandbox.Splices () where

import Sandbox.Templates (bigBadMathProblem, precompute)

$(precompute [1 .. 10])
