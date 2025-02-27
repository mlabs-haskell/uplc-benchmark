{-# LANGUAGE NoImplicitPrelude #-}

module UplcBenchmark.Utils (fromJustTrace, getOwnMint) where

import PlutusLedgerApi.V2 (CurrencySymbol, TokenName, Value, getValue)
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude (
  BuiltinString,
  Integer,
  Maybe (Just, Nothing),
  find,
  fst,
  snd,
  traceError,
  ($),
  (.),
  (==),
 )

{-# INLINE fromJustTrace #-}
fromJustTrace :: BuiltinString -> Maybe a -> a
fromJustTrace _ (Just a) = a
fromJustTrace err Nothing = traceError err

{-# INLINE getOwnMint #-}
getOwnMint :: CurrencySymbol -> Value -> [(TokenName, Integer)]
getOwnMint ownSymbol mint =
  Map.toList
    $ snd
    $ fromJustTrace "Could not find own mint"
    $ find ((== ownSymbol) . fst)
    $ Map.toList
    $ getValue mint
