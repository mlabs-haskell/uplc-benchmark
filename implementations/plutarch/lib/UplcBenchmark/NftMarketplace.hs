module UplcBenchmark.NftMarketplace (pnftMarketplaceValidator) where

import LambdaBuffers.NftMarketplace.Plutarch (
  NftMarketplaceDatum (NftMarketplaceDatum),
  NftMarketplaceRedeemer (
    NftMarketplaceRedeemer'Buy,
    NftMarketplaceRedeemer'Cancel
  ),
 )
import Plutarch.Api.V1.Value (pforgetPositive)
import Plutarch.Api.V2 (
  PDatum (PDatum),
  POutputDatum (POutputDatum),
  PScriptPurpose (PSpending),
  PTxOut,
  PValidator,
 )
import Plutarch.Builtin (pforgetData)
import Plutarch.Monadic qualified as P

pverifyData ::
  forall (a :: PType) (s :: S).
  (PTryFrom PData (PAsData a)) =>
  Term s PData ->
  Term s (PAsData a)
pverifyData = unTermCont . fmap fst . tcont . ptryFrom

passert ::
  forall (a :: PType) (s :: S).
  Term s PString ->
  Term s PBool ->
  Term s a ->
  Term s a
passert msg cond x = pif cond x $ ptraceError msg

pnftMarketplaceValidator :: ClosedTerm PValidator
pnftMarketplaceValidator = plam $ \rawDatum rawRedeemer ctx' -> P.do
  (NftMarketplaceDatum price seller cancelKey) <-
    pmatch $ pfromData $ pverifyData @NftMarketplaceDatum rawDatum
  redeemer <- plet $ pfromData $ pverifyData @NftMarketplaceRedeemer rawRedeemer
  ctx <- pletFields @'["txInfo", "purpose"] ctx'

  pmatch redeemer $ \case
    NftMarketplaceRedeemer'Buy -> P.do
      txInfo <- pletFields @'["outputs"] ctx.txInfo
      PSpending ownInput <- pmatch $ ctx.purpose
      ownInputRef <- plet $ pfield @"_0" # ownInput
      paymentDatum <- plet $ pcon $ PDatum $ pforgetData ownInputRef

      isPaymentUtxo <- plet $ plam $ \output' -> P.do
        output <- pletFields @'["value", "datum", "address"] @PTxOut output'
        (output.address #== seller)
          #&& (pdata (pforgetPositive output.value) #== price)
          #&& ( pmatch output.datum $ \case
                  POutputDatum inlineDatum -> pfield @"outputDatum" # inlineDatum #== paymentDatum
                  _ -> pconstant False
              )

      let hasValidPayment = pany # isPaymentUtxo # txInfo.outputs
      passert "Must have a valid payment" hasValidPayment
      popaque $ pconstant ()
    NftMarketplaceRedeemer'Cancel -> P.do
      txInfo <- pletFields @'["signatories"] ctx.txInfo
      let signedByCancelKey = pany # (plam $ \key -> cancelKey #== key) # txInfo.signatories
      passert "Must be signed by cancel key" signedByCancelKey
      popaque $ pconstant ()
