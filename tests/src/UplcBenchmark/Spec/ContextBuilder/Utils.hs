{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module UplcBenchmark.Spec.ContextBuilder.Utils where

import Data.ByteString qualified as ByteString
import Data.ByteString.Hash (blake2b_256)
import Data.Word (Word8)
import Optics (set)
import Plutarch.Context (UTXO)
import Plutarch.Context.Base (DatumType (ContextDatum, InlineDatum))
import PlutusLedgerApi.V2 (
  BuiltinByteString,
  CurrencySymbol (CurrencySymbol),
  ToData,
  TokenName (TokenName),
  toBuiltin,
  toData,
 )

mkHash32 :: Word8 -> BuiltinByteString
mkHash32 idx = toBuiltin $ blake2b_256 $ ByteString.pack [idx]

mkHash28 :: Word8 -> BuiltinByteString
mkHash28 idx = toBuiltin $ ByteString.take 28 $ blake2b_256 $ ByteString.pack [idx]

junkSymbol :: CurrencySymbol
junkSymbol = CurrencySymbol $ mkHash28 101

junkToken :: TokenName
junkToken = TokenName $ mkHash32 102

withRedeemer :: (ToData redeemer) => redeemer -> UTXO
withRedeemer r = set #redeemer (Just $ toData r) (mempty :: UTXO)

withInlineDatum :: (ToData datum) => datum -> UTXO
withInlineDatum dat = set #data (pure . InlineDatum . toData $ dat) (mempty :: UTXO)

withHashDatum :: (ToData datum) => datum -> UTXO
withHashDatum dat = set #data (pure . ContextDatum . toData $ dat) (mempty :: UTXO)
