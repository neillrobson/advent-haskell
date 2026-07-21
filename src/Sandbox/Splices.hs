module Sandbox.Splices () where

import Sandbox.Templates (precompute)

$(precompute [1 .. 10])
