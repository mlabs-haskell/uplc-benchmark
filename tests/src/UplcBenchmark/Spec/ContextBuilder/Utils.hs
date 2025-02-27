{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module UplcBenchmark.Spec.ContextBuilder.Utils where

import Crypto.Hash (Blake2b_256 (Blake2b_256), hashWith)

import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word8)
import Plutarch.LedgerApi.V3 (scriptHash)
import Plutarch.Script (Script)
import PlutusLedgerApi.V2 (
  BuiltinByteString,
  CurrencySymbol (CurrencySymbol),
  ScriptHash (getScriptHash),
  TokenName (TokenName),
  toBuiltin,
 )

blake2b_256 :: ByteString -> ByteString
blake2b_256 = convert @_ @ByteString . hashWith Blake2b_256

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
