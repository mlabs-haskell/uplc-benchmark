module UplcBenchmark.LpMintingPolicy (plpMintingPolicy) where

import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2 (
  PCurrencySymbol,
  PMintingPolicy,
  PScriptPurpose (PMinting),
  PTokenName,
  PTxInInfo,
 )
import Plutarch.Monadic qualified as P

import UplcBenchmark.Utils (pallBoth, passert, ptryGetOwnMint)

plpMintingPolicy :: ClosedTerm (PAsData PCurrencySymbol :--> PMintingPolicy)
plpMintingPolicy = plam $ \poolNftCs _redeemer ctx' -> P.do
  ctx <- pletFields @'["txInfo", "purpose"] ctx'

  PMinting ownSymbol' <- pmatch ctx.purpose
  ownSymbol <- plet $ pfromData $ pfield @"_0" # ownSymbol'

  inputHasNft :: Term s (PTokenName :--> PTxInInfo :--> PBool) <- plet $ plam $ \poolNftTn txInInfo' -> P.do
    txInInfo <- pletFields @'["resolved"] txInInfo'
    txOut <- pletFields @'["value"] txInInfo.resolved
    pvalueOf # txOut.value # pfromData poolNftCs # poolNftTn #== 1

  txInfo <- pletFields @'["inputs", "mint"] ctx.txInfo
  ownMint <- plet $ ptryGetOwnMint # ownSymbol # txInfo.mint

  isValidMint :: Term s (PTokenName :--> PInteger :--> PBool) <- plet $ plam $ \mintedLpName _mintedLpAmount ->
    -- brackets are redundant, but formatter is horrible without
    pany # (inputHasNft # mintedLpName) # txInfo.inputs

  -- For each entry in minting list, we must mint exactly one token (so no burning as well)
  -- and spend an input with corresponding NFT
  passert "NFT corresponding to LP must be spent" (pallBoth # isValidMint # ownMint)

  popaque $ pconstant ()
