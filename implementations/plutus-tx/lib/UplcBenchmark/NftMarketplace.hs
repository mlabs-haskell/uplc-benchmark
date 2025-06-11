{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}

module UplcBenchmark.NftMarketplace (nftMarketplaceValidator) where

import Data.Kind (Type)
import PlutusLedgerApi.V3 (
  Address,
  BuiltinData,
  Datum (Datum),
  FromData (fromBuiltinData),
  OutputDatum (OutputDatum),
  PubKeyHash,
  ScriptContext (scriptContextRedeemer, scriptContextScriptInfo, scriptContextTxInfo),
  ScriptInfo (SpendingScript),
  ToData (toBuiltinData),
  TxInfo (txInfoOutputs, txInfoSignatories),
  TxOut (txOutAddress, txOutDatum, txOutValue),
  UnsafeFromData (unsafeFromBuiltinData),
  Value,
  getDatum,
  getRedeemer,
 )
import PlutusTx.Builtins (chooseData, unsafeDataAsI, unsafeDataAsList)
import PlutusTx.List (any, elem)
import PlutusTx.Prelude (
  Applicative ((<*>)),
  Bool (False),
  BuiltinUnit,
  Eq ((==)),
  Maybe (Just, Nothing),
  check,
  const,
  traceIfFalse,
  ($),
  (&&),
  (<$>),
 )
import UplcBenchmark.Utils (fromJustTrace)

type NftMarketplaceDatum :: Type
data NftMarketplaceDatum = NftMarketplaceDatum
  { price :: Value
  , seller :: Address
  , cancelKey :: PubKeyHash
  }

instance FromData NftMarketplaceDatum where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d =
    chooseData
      d
      (const Nothing)
      (const Nothing)
      ( \d' ->
          case unsafeDataAsList d' of
            [price, seller, cancelKey] ->
              NftMarketplaceDatum
                <$> fromBuiltinData price
                <*> fromBuiltinData seller
                <*> fromBuiltinData cancelKey
            _ -> Nothing
      )
      (const Nothing)
      (const Nothing)
      d

type NftMarketplaceRedeemer :: Type
data NftMarketplaceRedeemer
  = NftMarketplaceRedeemer'Buy
  | NftMarketplaceRedeemer'Cancel

instance FromData NftMarketplaceRedeemer where
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
                then Just NftMarketplaceRedeemer'Buy
                else
                  if i == 1
                    then Just NftMarketplaceRedeemer'Cancel
                    else Nothing
      )
      (const Nothing)
      d

{-# INLINE nftMarketplaceValidator #-}
nftMarketplaceValidator :: BuiltinData -> BuiltinUnit
nftMarketplaceValidator rawCtx =
  let
    !ctx :: ScriptContext = unsafeFromBuiltinData rawCtx

    -- We're not using unsafeFromBuiltinData because in plutarch implementation
    -- we're performing these checks as well.

    !redeemer :: NftMarketplaceRedeemer =
      fromJustTrace "Redeemer decoding failed" $ fromBuiltinData $ getRedeemer $ scriptContextRedeemer ctx

    SpendingScript !ownInput rawDatum = scriptContextScriptInfo ctx

    Just rawDatum' = rawDatum

    !datum :: NftMarketplaceDatum =
      fromJustTrace "Datum decoding failed" $ fromBuiltinData $ getDatum rawDatum'

    NftMarketplaceDatum !price !seller !cancelKey = datum

    !txInfo = scriptContextTxInfo ctx
   in
    check $ case redeemer of
      NftMarketplaceRedeemer'Buy ->
        let
          !outputs = txInfoOutputs txInfo
          !paymentDatum = toBuiltinData ownInput

          isPaymentUtxo utxo =
            (txOutAddress utxo == seller)
              && (txOutValue utxo == price)
              && ( case txOutDatum utxo of
                     OutputDatum inlineDatum -> inlineDatum == Datum paymentDatum
                     _ -> False
                 )

          !hasValidPayment = any isPaymentUtxo outputs
         in
          traceIfFalse "Must have a valid payment" hasValidPayment
      NftMarketplaceRedeemer'Cancel ->
        let
          !signatories = txInfoSignatories txInfo
          !signedByCancelKey = elem cancelKey signatories
         in
          traceIfFalse "Must be signed by cancel key" signedByCancelKey
