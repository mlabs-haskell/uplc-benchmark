{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module UplcBenchmark.LpMintingPolicy (lpMintingPolicy) where

import PlutusLedgerApi.V1.Value (valueOf)
import PlutusLedgerApi.V2 (
  BuiltinData,
  CurrencySymbol,
  ScriptContext (scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting),
  TokenName,
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint),
  TxOut (txOutValue),
  UnsafeFromData (unsafeFromBuiltinData),
 )
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
lpMintingPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit
lpMintingPolicy rawPoolNftCs _rawRedeemer rawCtx =
  let
    !ctx :: ScriptContext = unsafeFromBuiltinData rawCtx
    !poolNftCs :: CurrencySymbol = unsafeFromBuiltinData rawPoolNftCs
    Minting !ownSymbol = scriptContextPurpose ctx

    inputHasNft :: TokenName -> TxInInfo -> Bool
    inputHasNft poolNftTn txIn =
      let !txOut = txInInfoResolved txIn
       in valueOf (txOutValue txOut) poolNftCs poolNftTn == 1

    !txInfo = scriptContextTxInfo ctx
    !inputs = txInfoInputs txInfo
    !mint = txInfoMint txInfo

    isValidMint :: (TokenName, Integer) -> Bool
    isValidMint (mintedLpName, _mintedLpAmount) =
      any (inputHasNft mintedLpName) inputs

    !ownMint = getOwnMint ownSymbol mint

    validMint :: Bool
    !validMint = all isValidMint ownMint
   in
    check $ traceIfFalse "NFT corresponding to LP must be spent" validMint
