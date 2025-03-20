{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}

module UplcBenchmark.PoolValidator (poolValidator) where

import Data.Kind (Type)
import PlutusLedgerApi.V1.Value (AssetClass (unAssetClass), assetClassValueOf, valueOf)
import PlutusLedgerApi.V3 (
  BuiltinData,
  CurrencySymbol,
  Datum (getDatum),
  FromData (fromBuiltinData),
  OutputDatum (OutputDatum),
  ScriptContext (scriptContextRedeemer, scriptContextScriptInfo, scriptContextTxInfo),
  ScriptInfo (SpendingScript),
  TxInInfo (txInInfoOutRef, txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint, txInfoOutputs),
  TxOut (txOutDatum, txOutValue),
  Value (Value),
  fromBuiltinData,
  getRedeemer,
  unsafeFromBuiltinData,
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.Builtins (chooseData, unsafeDataAsI, unsafeDataAsList)
import PlutusTx.Prelude (
  Applicative ((<*>)),
  Bool,
  BuiltinUnit,
  Integer,
  Maybe (Just, Nothing),
  check,
  const,
  find,
  snd,
  traceIfFalse,
  ($),
  (&&),
  (*),
  (+),
  (-),
  (.),
  (<),
  (<$>),
  (<=),
  (==),
  (>),
 )
import UplcBenchmark.Utils (fromJustTrace)

type DexDatum :: Type
data DexDatum = DexDatum
  { tokenA :: AssetClass
  , tokenB :: AssetClass
  , poolNft :: AssetClass
  , lpToken :: CurrencySymbol
  , mintedLpTokens :: Integer
  , swapFee :: Integer
  }

instance FromData DexDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d =
    chooseData
      d
      (const Nothing)
      (const Nothing)
      ( \d' ->
          case unsafeDataAsList d' of
            [tokenA, tokenB, poolNft, lpToken, mintedLpTokens, swapFee] ->
              DexDatum
                <$> fromBuiltinData tokenA
                <*> fromBuiltinData tokenB
                <*> fromBuiltinData poolNft
                <*> fromBuiltinData lpToken
                <*> fromBuiltinData mintedLpTokens
                <*> fromBuiltinData swapFee
            _ -> Nothing
      )
      (const Nothing)
      (const Nothing)
      d

type DexRedeemer :: Type
data DexRedeemer
  = DexRedeemer'Swap
  | DexRedeemer'DepositLiquidity
  | DexRedeemer'WithdrawLiquidity

instance FromData DexRedeemer where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d =
    chooseData
      d
      (const Nothing)
      (const Nothing)
      (const Nothing)
      ( \d' ->
          let i = unsafeDataAsI d'
           in if i == 0
                then Just DexRedeemer'Swap
                else
                  if i == 1
                    then Just DexRedeemer'DepositLiquidity
                    else
                      if i == 2
                        then Just DexRedeemer'WithdrawLiquidity
                        else Nothing
      )
      (const Nothing)
      d

{-# INLINE poolValidator #-}
poolValidator :: BuiltinData -> BuiltinUnit
poolValidator rawCtx =
  let
    !redeemer :: DexRedeemer =
      fromJustTrace "Redeemer decoding failed"
        $ fromBuiltinData
        $ getRedeemer
        $ scriptContextRedeemer ctx
    SpendingScript !ownInputRef rawDatum = scriptContextScriptInfo ctx
    Just rawDatum' = rawDatum
    !inDatum :: DexDatum = fromJustTrace "Datum decoding failed" $ fromBuiltinData $ getDatum rawDatum'
    !ctx :: ScriptContext = unsafeFromBuiltinData rawCtx

    !txInfo = scriptContextTxInfo ctx
    !inputs = txInfoInputs txInfo
    !outputs = txInfoOutputs txInfo
    UnsafeMintValue mint = txInfoMint txInfo

    DexDatum !inTokenA !inTokenB !inPoolNft !inLpToken !inMintedLpTokens !inSwapFee = inDatum

    !nftAndLpTokenName = snd $ unAssetClass inPoolNft

    !ownInput =
      txInInfoResolved
        $ fromJustTrace "Cannot find own input"
        $ find ((== ownInputRef) . txInInfoOutRef) inputs

    !newMintedLp = valueOf (Value mint) inLpToken nftAndLpTokenName

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

{-# INLINEABLE checkSwap #-}
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
