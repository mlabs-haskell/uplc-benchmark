module UplcBenchmark.Utils (
  ptryDecodeData,
  passert,
  pallBoth,
  ptryGetOwnMint,
  pintegerToByteString,
  ptryFind,
  ptryFindInput,
  pvalueOfAssetClass,
  ptryFindOutputWithAsset,
  pisPDNothing,
)
where

import Data.Kind (Type)
import Plutarch.LedgerApi.AssocMap (PMap, pempty, pfindWithDefault, pforgetSorted)
import Plutarch.LedgerApi.V2 (
  PCurrencySymbol,
  PMaybeData (PDJust, PDNothing),
  PTokenName,
  PTxInInfo (PTxInInfo),
  PTxOut (PTxOut),
  PTxOutRef,
  PValue,
 )
import Plutarch.LedgerApi.Value (PAssetClass (PAssetClass), pvalueOf)

ptryDecodeData ::
  forall (a :: S -> Type) (s :: S).
  (PTryFrom PData (PAsData a), PIsData a) =>
  Term s PData ->
  Term s a
ptryDecodeData = pfromData . unTermCont . fmap fst . tcont . ptryFrom

passert ::
  forall (a :: S -> Type) (s :: S).
  Term s PString ->
  Term s PBool ->
  Term s a ->
  Term s a
passert msg cond x = pif cond x $ ptraceInfoError msg

pallBoth ::
  (PIsData k, PIsData v) =>
  Term s ((k :--> v :--> PBool) :--> PMap any k v :--> PBool)
pallBoth = phoistAcyclic $
  plam $ \predicate m ->
    pall # plam (\pair -> predicate # pfromData (pfstBuiltin # pair) # pfromData (psndBuiltin # pair)) # pto m

{- | Throws error if own currency is not present in mint map, which is unreachable
| if values are from a valid script context
-}
ptryGetOwnMint :: ClosedTerm (PCurrencySymbol :--> PValue anyKey anyValue :--> PMap anyKey PTokenName PInteger)
ptryGetOwnMint = phoistAcyclic $
  plam $ \ownCurrency mint ->
    pfindWithDefault # pforgetSorted pempty # ownCurrency # pto mint

-- | Convert Integer to ByteString.
pintegerToByteString :: ClosedTerm (PInteger :--> PByteString)
pintegerToByteString = phoistAcyclic $ plam $ \n -> pfix # plam go # n # mempty
  where
    go ::
      Term s (PInteger :--> PByteString :--> PByteString) ->
      Term s PInteger ->
      Term s (PByteString :--> PByteString)
    go self n = plam $ \rest -> FooBar.do
      pif
        (n #< 256)
        (pconsBS # (pintegerToByte # n) # rest)
        (self # (pdiv # n # 256) #$ pconsBS # (pintegerToByte # (pmod # n # 256)) # rest)

-- | Like `pfind` but throws an error if doesn't find anything
ptryFind :: forall a l s. (PIsListLike l a) => Term s ((a :--> PBool) :--> l a :--> a)
ptryFind = phoistAcyclic $
  pfix #$ plam $ \self f xs ->
    pelimList
      (\y ys -> pif (f # y) y (self # f # ys))
      (ptraceInfoError "ptryFind: no match")
      xs

ptryFindInput :: Term s (PTxOutRef :--> PBuiltinList (PAsData PTxInInfo) :--> PAsData PTxInInfo)
ptryFindInput = phoistAcyclic $
  plam $ \outRef lst ->
    ptryFind @(PAsData PTxInInfo) # plam (\txInInfo -> pmatch (pfromData txInInfo) $ \(PTxInInfo outRef' _) -> outRef' #== outRef) # lst

pvalueOfAssetClass :: Term s (PValue anyKey anyAmount :--> PAssetClass :--> PInteger)
pvalueOfAssetClass = phoistAcyclic $ plam $ \value asset' -> pmatch asset' $ \(PAssetClass asset) -> P.do
  pvalueOf # value # (pfromData $ pfstBuiltin # asset) # (pfromData $ psndBuiltin # asset)

ptryFindOutputWithAsset :: Term s (PAssetClass :--> PBuiltinList (PAsData PTxOut) :--> PAsData PTxOut)
ptryFindOutputWithAsset = phoistAcyclic $ plam $ \token outputs -> P.do
  ptryFind # plam (\txOut -> pmatch (pfromData txOut) $ \(PTxOut _ txOutValue _ _) -> pvalueOfAssetClass # pfromData txOutValue # token #> 0) # outputs

pisPDNothing :: Term s (PMaybeData a :--> PBool)
pisPDNothing = phoistAcyclic $ plam $ \m -> pmatch m $ \case
  PDNothing -> pconstant True
  PDJust _ -> pconstant False
