{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module UplcBenchmark.NftMarketplace (nftMarketplaceValidator) where

import LambdaBuffers.NftMarketplace (
  NftMarketplaceDatum (NftMarketplaceDatum),
  NftMarketplaceRedeemer (NftMarketplaceRedeemer'Buy, NftMarketplaceRedeemer'Cancel),
 )
import PlutusLedgerApi.V2 (
  BuiltinData,
  Datum (Datum),
  FromData (fromBuiltinData),
  OutputDatum (OutputDatum),
  ScriptContext (scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Spending),
  ToData (toBuiltinData),
  TxInfo (txInfoOutputs, txInfoSignatories),
  TxOut (txOutAddress, txOutDatum, txOutValue),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Base (($))
import PlutusTx.Bool (Bool (False), (&&))
import PlutusTx.Maybe (Maybe (Just, Nothing))
import PlutusTx.Prelude (BuiltinString, Eq ((==)), any, check, elem, traceError, traceIfFalse)

{-# INLINE fromJustTrace #-}
fromJustTrace :: BuiltinString -> Maybe a -> a
fromJustTrace _ (Just a) = a
fromJustTrace err Nothing = traceError err

{-# INLINE nftMarketplaceValidator #-}
nftMarketplaceValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
nftMarketplaceValidator rawDatum rawRedeemer rawCtx =
  let
    !ctx :: ScriptContext = unsafeFromBuiltinData rawCtx

    -- We're not using unsafeFromBuiltinData because in plutarch implementation
    -- we're performing these checks as well.

    !redeemer :: NftMarketplaceRedeemer =
      fromJustTrace "Redeemer decoding failed" $ fromBuiltinData rawRedeemer

    !datum :: NftMarketplaceDatum =
      fromJustTrace "Datum decoding failed" $ fromBuiltinData rawDatum

    NftMarketplaceDatum !price !seller !cancelKey = datum

    !txInfo = scriptContextTxInfo ctx
   in
    check $ case redeemer of
      NftMarketplaceRedeemer'Buy ->
        let
          !outputs = txInfoOutputs txInfo
          Spending !ownInput = scriptContextPurpose ctx
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
