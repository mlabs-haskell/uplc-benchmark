module UplcBenchmark.NftMarketplace (pnftMarketplaceValidator) where

import Data.Kind (Type)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (
  AmountGuarantees (NoGuarantees, Positive),
  KeyGuarantees (Sorted, Unsorted),
  PAddress,
  PDatum (PDatum),
  PMaybeData (PDJust),
  POutputDatum (POutputDatum),
  PPubKeyHash,
  PScriptContext (PScriptContext),
  PScriptInfo (PSpendingScript),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
 )
import Plutarch.LedgerApi.Value (PValue)
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)
import UplcBenchmark.Utils (passert, punsafeCastDatum, punsafeCastRedeemer)

type NftMarketplaceDatum :: S -> Type
data NftMarketplaceDatum s = NftMarketplaceDatum
  { price :: Term s (PAsData (PValue 'Unsorted 'NoGuarantees))
  , seller :: Term s (PAsData PAddress)
  , cancelKey :: Term s (PAsData PPubKeyHash)
  }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, PEq, PIsData)
  deriving (PlutusType) via (DeriveAsDataRec NftMarketplaceDatum)

type NftMarketplaceRedeemer :: S -> Type
data NftMarketplaceRedeemer s
  = NftMarketplaceRedeemer'Buy
  | NftMarketplaceRedeemer'Cancel
  deriving stock (GHC.Generic, Show)
  deriving anyclass (SOP.Generic, PEq, PIsData)
  deriving (PlutusType) via DeriveTagPlutusType NftMarketplaceRedeemer

pnftMarketplaceValidator :: ClosedTerm (PAsData PScriptContext :--> POpaque)
pnftMarketplaceValidator = plam $ \ctx' -> P.do
  PScriptContext txInfo redeemer info <- pmatch (pfromData ctx')
  PSpendingScript ownInput datum' <- pmatch info
  PDJust datum <- pmatch datum'
  NftMarketplaceDatum price seller cancelKey <- pmatch (pfromData $ punsafeCastDatum $ pfromData datum)

  pmatch (pfromData $ punsafeCastRedeemer redeemer) $ \case
    NftMarketplaceRedeemer'Buy -> P.do
      PTxInfo _ _ outputs _ _ _ _ _ _ _ _ _ _ _ _ _ <- pmatch txInfo

      paymentDatum <- plet $ pcon $ PDatum $ pto $ pdata ownInput

      isPaymentUtxo <- plet $ plam $ \output -> pmatch (pfromData output) $ \(PTxOut outputAddress outputValue outputDatum _) -> P.do
        (pdata outputAddress #== seller)
          #&& (pforgetSortedPositiveData outputValue #== price)
          #&& ( pmatch outputDatum $ \case
                  POutputDatum inlineDatum -> inlineDatum #== paymentDatum
                  _ -> pconstant False
              )

      let hasValidPayment = pany # isPaymentUtxo # pfromData outputs
      passert "Must have a valid payment" hasValidPayment
      popaque $ pconstant @PUnit ()
    NftMarketplaceRedeemer'Cancel -> P.do
      PTxInfo _ _ _ _ _ _ _ _ signatories _ _ _ _ _ _ _ <- pmatch txInfo
      let signedByCancelKey = pany # (plam $ \key -> cancelKey #== key) # pfromData signatories
      passert "Must be signed by cancel key" signedByCancelKey
      popaque $ pconstant @PUnit ()

pforgetSortedPositiveData ::
  Term s (PAsData (PValue 'Sorted 'Positive)) ->
  Term s (PAsData (PValue 'Unsorted 'NoGuarantees))
pforgetSortedPositiveData = punsafeCoerce
