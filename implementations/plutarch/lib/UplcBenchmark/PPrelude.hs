{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Description: Prelude imported throughout this project

Prelude imported throughout this project
-}
module UplcBenchmark.PPrelude (
  module Prelude,
  module Plutarch.Prelude,
  module Plutarch,
  (#>=),
  (#>),
) where

import Data.ByteString (ByteString)
import GHC.Exts (fromListN)
import Plutarch (compile)
import Plutarch.Prelude

-- | Flipped version of '(#<=)'
a #>= b = b #<= a

infix 4 #>=

-- | Flipped version of '(#<)'
a #> b = b #< a

infix 4 #>
