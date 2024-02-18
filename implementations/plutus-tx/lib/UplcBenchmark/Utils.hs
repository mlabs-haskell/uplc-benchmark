{-# LANGUAGE NoImplicitPrelude #-}

module UplcBenchmark.Utils (fromJustTrace) where

import PlutusTx.Prelude (BuiltinString, Maybe (Just, Nothing), traceError)

{-# INLINE fromJustTrace #-}
fromJustTrace :: BuiltinString -> Maybe a -> a
fromJustTrace _ (Just a) = a
fromJustTrace err Nothing = traceError err
