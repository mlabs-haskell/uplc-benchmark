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

import LambdaBuffers.Plutus.V1.Plutarch (AssetClass)
import Plutarch.Api.V1.AssocMap (PMap, pfindWithDefault)
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2 (
  PCurrencySymbol,
  PMaybeData (PDJust, PDNothing),
  PTokenName,
  PTxInInfo,
  PTxOut,
  PTxOutRef,
  PValue,
 )
import Plutarch.Monadic qualified as P

ptryDecodeData ::
  forall (a :: PType) (s :: S).
  (PTryFrom PData (PAsData a), PIsData a) =>
  Term s PData ->
  Term s a
ptryDecodeData = pfromData . unTermCont . fmap fst . tcont . ptryFrom

passert ::
  forall (a :: PType) (s :: S).
  Term s PString ->
  Term s PBool ->
  Term s a ->
  Term s a
passert msg cond x = pif cond x $ ptraceError msg

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
    pfindWithDefault # ptraceError "unreachable: only called on mint" # ownCurrency # pto mint

-- | Convert Integer to ByteString.
pintegerToByteString :: ClosedTerm (PInteger :--> PByteString)
pintegerToByteString = phoistAcyclic $ plam $ \n -> pfix # plam go # n # mempty
  where
    go ::
      Term s (PInteger :--> PByteString :--> PByteString) ->
      Term s PInteger ->
      Term s (PByteString :--> PByteString)
    go self n = plam $ \rest -> P.do
      pif
        (n #< 256)
        (pconsBS # n # rest)
        (self # (pdiv # n # 256) #$ pconsBS # (pmod # n # 256) # rest)

-- | Like `pfind` but throws an error if doesn't find anything
ptryFind :: (PIsListLike l a) => Term s ((a :--> PBool) :--> l a :--> a)
ptryFind = phoistAcyclic $
  pfix #$ plam $ \self f xs ->
    pelimList
      (\y ys -> pif (f # y) y (self # f # ys))
      (ptraceError "ptryFind: no match")
      xs

ptryFindInput :: Term s (PTxOutRef :--> PBuiltinList PTxInInfo :--> PTxInInfo)
ptryFindInput = phoistAcyclic $
  plam $ \outRef lst ->
    ptryFind # plam (\txInInfo -> pfield @"outRef" # txInInfo #== outRef) # lst

pvalueOfAssetClass :: Term s (PValue anyKey anyAmount :--> AssetClass :--> PInteger)
pvalueOfAssetClass = phoistAcyclic $ plam $ \value asset' -> P.do
  asset <- pletFields @'["_0", "_1"] asset'
  pvalueOf # value # asset._0 # asset._1

ptryFindOutputWithAsset :: Term s (AssetClass :--> PBuiltinList PTxOut :--> PTxOut)
ptryFindOutputWithAsset = phoistAcyclic $ plam $ \token outputs -> P.do
  ptryFind # plam (\txOut -> pvalueOfAssetClass # (pfield @"value" # txOut) # token #> 0) # outputs

pisPDNothing :: Term s (PMaybeData a :--> PBool)
pisPDNothing = phoistAcyclic $ plam $ \m -> pmatch m $ \case
  PDNothing _ -> pconstant True
  PDJust _ -> pconstant False
