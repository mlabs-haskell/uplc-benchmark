module UplcBenchmark.LpMintingPolicy (plpMintingPolicy) where

import Plutarch.LedgerApi.V2 (
  PCurrencySymbol,
  PScriptContext (PScriptContext),
  PScriptPurpose (PMinting),
  PTokenName,
  PTxInInfo (PTxInInfo),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
 )
import Plutarch.LedgerApi.Value (pvalueOf)
import Plutarch.Monadic qualified as P

import UplcBenchmark.Utils (pallBoth, passert, ptryGetOwnMint)

plpMintingPolicy :: ClosedTerm (PAsData PCurrencySymbol :--> PData :--> PAsData PScriptContext :--> POpaque)
plpMintingPolicy = plam $ \poolNftCs _redeemer ctx -> P.do
  PScriptContext txInfo purpose <- pmatch (pfromData ctx)

  PMinting ownSymbol <- pmatch purpose

  inputHasNft :: Term s (PTokenName :--> PAsData PTxInInfo :--> PBool) <- plet $ plam $ \poolNftTn txInInfo -> P.do
    (PTxInInfo _ resolved) <- pmatch (pfromData txInInfo)
    (PTxOut _ value _ _) <- pmatch resolved
    pvalueOf # pfromData value # pfromData poolNftCs # poolNftTn #== 1

  PTxInfo inputs _ _ _ mint _ _ _ _ _ _ _ <- pmatch txInfo
  ownMint <- plet $ ptryGetOwnMint # pfromData ownSymbol # pfromData mint

  isValidMint :: Term s (PTokenName :--> PInteger :--> PBool) <- plet $ plam $ \mintedLpName _mintedLpAmount ->
    pany # (inputHasNft # mintedLpName) # pfromData inputs

  -- For each entry in minting list, we must mint exactly one token (so no burning as well)
  -- and spend an input with corresponding NFT
  passert "NFT corresponding to LP must be spent" (pallBoth # isValidMint # ownMint)

  popaque $ pconstant @PUnit ()
