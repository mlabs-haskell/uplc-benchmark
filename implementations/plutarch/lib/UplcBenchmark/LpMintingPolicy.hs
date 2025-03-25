module UplcBenchmark.LpMintingPolicy (plpMintingPolicy) where

import Plutarch.LedgerApi.AssocMap (PMap, pempty, pfindWithDefault, pforgetSorted)
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol,
  PScriptContext (PScriptContext),
  PScriptInfo (PMintingScript),
  PTokenName,
  PTxInInfo (PTxInInfo),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
 )
import Plutarch.LedgerApi.Value (pvalueOf)
import Plutarch.Monadic qualified as P

import UplcBenchmark.Utils (passert)

plpMintingPolicy :: forall s. Term s (PAsData PCurrencySymbol :--> PAsData PScriptContext :--> POpaque)
plpMintingPolicy = plam $ \poolNftCs ctx -> P.do
  PScriptContext txInfo _ purpose <- pmatch (pfromData ctx)

  PMintingScript ownSymbol <- pmatch purpose

  PTxInfo inputs _ _ _ mint _ _ _ _ _ _ _ _ _ _ _ <- pmatch txInfo
  -- further optimization is possible by inlining pfindWithDefault
  let ownMint = pfindWithDefault # pforgetSorted pempty # pfromData ownSymbol # pto (pfromData mint)

  let
    inputHasNft :: Term s PTokenName -> Term s (PAsData PTxInInfo :--> PBool)
    inputHasNft poolNftTn = plam $ \txInInfo -> P.do
      (PTxInInfo _ resolved) <- pmatch (pfromData txInInfo)
      (PTxOut _ value _ _) <- pmatch resolved
      pvalueOf # pfromData value # pfromData poolNftCs # poolNftTn #== 1

    isValidMintPredicate :: Term s PTokenName -> Term s PBool
    isValidMintPredicate mintedLpName =
      pany # (inputHasNft mintedLpName) # pfromData inputs

    isValidMint :: forall v any. Term s (PMap any PTokenName v) -> Term s PBool
    isValidMint m = pall # plam (\pair -> isValidMintPredicate $ pfromData (pfstBuiltin # pair)) # pto m

  -- For each entry in minting list, we must mint exactly one token (so no burning as well)
  -- and spend an input with corresponding NFT
  passert "NFT corresponding to LP must be spent" (isValidMint ownMint)

  popaque $ pconstant @PUnit ()
