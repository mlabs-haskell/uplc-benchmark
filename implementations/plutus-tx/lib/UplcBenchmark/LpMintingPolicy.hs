{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module UplcBenchmark.LpMintingPolicy (lpMintingPolicy) where

import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V3 (
  BuiltinData,
  CurrencySymbol,
  ScriptContext (scriptContextScriptInfo, scriptContextTxInfo),
  ScriptInfo (MintingScript),
  TokenName,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint),
  TxOut (txOutValue),
  UnsafeFromData (unsafeFromBuiltinData),
  Value (Value),
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.Prelude (
  Bool,
  BuiltinUnit,
  Integer,
  all,
  any,
  check,
  traceIfFalse,
  ($),
  (==),
 )
import UplcBenchmark.Utils (getOwnMint)

{-# INLINE lpMintingPolicy #-}
lpMintingPolicy :: BuiltinData -> BuiltinData -> BuiltinUnit
lpMintingPolicy rawPoolNftCs rawCtx =
  let
    !ctx :: ScriptContext = unsafeFromBuiltinData rawCtx
    !poolNftCs :: CurrencySymbol = unsafeFromBuiltinData rawPoolNftCs
    MintingScript !ownSymbol = scriptContextScriptInfo ctx

    inputHasNft :: TokenName -> TxInInfo -> Bool
    inputHasNft poolNftTn txIn =
      let !txOut = txInInfoResolved txIn
       in valueOf (txOutValue txOut) poolNftCs poolNftTn == 1

    !txInfo = scriptContextTxInfo ctx
    !inputs = txInfoInputs txInfo
    UnsafeMintValue mint = txInfoMint txInfo

    isValidMint :: (TokenName, Integer) -> Bool
    isValidMint (mintedLpName, _mintedLpAmount) =
      any (inputHasNft mintedLpName) inputs

    !ownMint = getOwnMint ownSymbol (Value mint)

    validMint :: Bool
    !validMint = all isValidMint ownMint
   in
    check $ traceIfFalse "NFT corresponding to LP must be spent" validMint
