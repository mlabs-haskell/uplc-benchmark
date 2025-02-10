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
) where

import Data.ByteString (ByteString)
import GHC.Exts (fromListN)
import Plutarch.Prelude
