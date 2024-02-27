{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module UplcBenchmark.Spec.ContextBuilder.Utils where

import Data.ByteString qualified as ByteString
import Data.ByteString.Hash (blake2b_256)
import Data.Word (Word8)
import Plutarch (Script)
import Plutarch.Api.V2 (scriptHash)
import PlutusLedgerApi.V2 (
  BuiltinByteString,
  CurrencySymbol (CurrencySymbol),
  ScriptHash (getScriptHash),
  TokenName (TokenName),
  toBuiltin,
 )

mkHash32 :: Word8 -> BuiltinByteString
mkHash32 idx = toBuiltin $ blake2b_256 $ ByteString.pack [idx]

mkHash28 :: Word8 -> BuiltinByteString
mkHash28 idx = toBuiltin $ ByteString.take 28 $ blake2b_256 $ ByteString.pack [idx]

junkSymbol :: CurrencySymbol
junkSymbol = CurrencySymbol $ mkHash28 101

junkToken :: TokenName
junkToken = TokenName $ mkHash32 102

scriptSymbol :: Script -> CurrencySymbol
scriptSymbol = CurrencySymbol . getScriptHash . scriptHash
