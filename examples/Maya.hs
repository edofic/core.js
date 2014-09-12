module Maya where

import GHC.Types
import GHC.Prim

maya = IO (\w -> (# w, () #))

