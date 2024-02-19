{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module UplcBenchmark.PoolValidator (poolValidator) where

import LambdaBuffers.Dex (
  DexDatum (DexDatum),
  DexRedeemer (DexRedeemer'DepositLiquidity, DexRedeemer'Swap, DexRedeemer'WithdrawLiquidity),
 )
import PlutusLedgerApi.V1.Value (AssetClass (unAssetClass), assetClassValueOf, valueOf)
import PlutusLedgerApi.V2 (
  BuiltinData,
  Datum (getDatum),
  OutputDatum (OutputDatum),
  ScriptContext (scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Spending),
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint, txInfoOutputs),
  TxOut (txOutDatum, txOutValue),
  fromBuiltinData,
  unsafeFromBuiltinData,
 )
import PlutusTx.Prelude (Bool, Integer, check, find, snd, traceIfFalse, ($), (&&), (*), (+), (-), (.), (<), (<=), (==), (>))
import UplcBenchmark.Utils (fromJustTrace)

{-# INLINE poolValidator #-}
poolValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
poolValidator rawDatum rawRedeemer rawCtx =
  let
    !redeemer :: DexRedeemer = fromJustTrace "Redeemer decoding failed" $ fromBuiltinData rawRedeemer
    !inDatum :: DexDatum = fromJustTrace "Datum decoding failed" $ fromBuiltinData rawDatum
    !ctx :: ScriptContext = unsafeFromBuiltinData rawCtx

    !txInfo = scriptContextTxInfo ctx
    !inputs = txInfoInputs txInfo
    !outputs = txInfoOutputs txInfo
    !mint = txInfoMint txInfo

    DexDatum !inTokenA !inTokenB !inPoolNft !inLpToken !inMintedLpTokens !inSwapFee = inDatum

    !nftAndLpTokenName = snd $ unAssetClass inPoolNft

    Spending !ownInputRef = scriptContextPurpose ctx
    !ownInput =
      txInInfoResolved
        $ fromJustTrace "Cannot find own input"
        $ find ((== ownInputRef) . txInInfoOutRef) inputs

    !newMintedLp = valueOf mint inLpToken nftAndLpTokenName

    !ownOutput =
      fromJustTrace "Cannot find own output"
        $ find (\txOut -> assetClassValueOf (txOutValue txOut) inPoolNft == 1) outputs

    OutputDatum !outDatumRaw = txOutDatum ownOutput
    !outDatum :: DexDatum =
      fromJustTrace "Cannot decode output datum"
        $ fromBuiltinData
        $ getDatum outDatumRaw
    DexDatum !outTokenA !outTokenB !outPoolNft !outLpToken !outMintedLpTokens !outSwapFee = outDatum

    !inAAmount = assetClassValueOf (txOutValue ownInput) inTokenA
    !inBAmount = assetClassValueOf (txOutValue ownInput) inTokenB

    !outAAmount = assetClassValueOf (txOutValue ownOutput) inTokenA
    !outBAmount = assetClassValueOf (txOutValue ownOutput) inTokenB

    !validTokenA = traceIfFalse "Invalid token A" $ inTokenA == outTokenA
    !validTokenB = traceIfFalse "Invalid token B" $ inTokenB == outTokenB
    !validPoolNft = traceIfFalse "Invalid pool NFT" $ inPoolNft == outPoolNft
    !validLpToken = traceIfFalse "Invalid LP token" $ inLpToken == outLpToken
    !validSwapFee = traceIfFalse "Invalid swap fee" $ inSwapFee == outSwapFee
    !commonChecks =
      traceIfFalse "Invalid common checks"
        $ validTokenA
        && validTokenB
        && validPoolNft
        && validLpToken
        && validSwapFee
   in
    check $ case redeemer of
      DexRedeemer'DepositLiquidity ->
        let
          !invalidMintedLp =
            traceIfFalse
              "Invalid minted LP"
              ((inMintedLpTokens + newMintedLp) == outMintedLpTokens)

          !notBurn =
            traceIfFalse
              "Must not burn LP tokens"
              (newMintedLp > 0)

          !depositedEnough =
            traceIfFalse
              "Not deposited enough"
              (outMintedLpTokens * outMintedLpTokens <= outAAmount * outBAmount)
         in
          commonChecks && invalidMintedLp && notBurn && depositedEnough
      DexRedeemer'WithdrawLiquidity ->
        let
          !invalidMintedLp =
            traceIfFalse
              "Invalid minted LP"
              ((inMintedLpTokens + newMintedLp) == outMintedLpTokens)

          !notMint =
            traceIfFalse
              "Must not mint LP tokens"
              (newMintedLp < 0)

          !withdrawnEnough =
            traceIfFalse
              "Withdrawn too much"
              (outMintedLpTokens * outMintedLpTokens <= outAAmount * outBAmount)
         in
          commonChecks && invalidMintedLp && notMint && withdrawnEnough
      DexRedeemer'Swap ->
        let
          !notMint =
            traceIfFalse
              "Must not mint LP tokens"
              (newMintedLp == 0)

          !invalidMintedLp =
            traceIfFalse
              "Invalid output datum: mintedLpTokens"
              (inMintedLpTokens == outMintedLpTokens)

          !validSwap =
            traceIfFalse "Swapped too much"
              $ checkSwap inSwapFee inAAmount inBAmount outAAmount outBAmount
         in
          commonChecks && invalidMintedLp && notMint && validSwap

checkSwap ::
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Integer ->
  Bool
checkSwap feeNum oldA oldB newA newB =
  feeDen * feeDen * oldA * oldB <= ((newA * feeDen) - (getDiff newA oldA * feeNum)) * ((newB * feeDen) - (getDiff newB oldB * feeNum))

{-# INLINE feeDen #-}
feeDen :: Integer
feeDen = 1000

{-# INLINE getDiff #-}
getDiff :: Integer -> Integer -> Integer
getDiff new old = if new - old < 0 then 0 else new - old
