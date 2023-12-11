module UplcBenchmark.NftMintingPolicy (pnftMintingPolicy) where

import LambdaBuffers.Dex.Plutarch (
  NftMintingPolicyRedeemer (
    NftMintingPolicyRedeemer'CreatePool
  ),
 )
import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptPurpose (PMinting),
  PTokenName (PTokenName),
  PTxInInfo,
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P

import UplcBenchmark.Utils (
  passert,
  pintegerToByteString,
  ptryDecodeData,
  ptryGetOwnMint,
 )

pnftMintingPolicy :: ClosedTerm PMintingPolicy
pnftMintingPolicy = plam $ \rawRedeemer ctx' -> P.do
  NftMintingPolicyRedeemer'CreatePool initialSpend' <-
    pmatch $ ptryDecodeData @NftMintingPolicyRedeemer rawRedeemer
  initialSpend <- plet $ pfromData initialSpend'

  expectedNftName <- plet $ pderiveNftName # initialSpend

  ctx <- pletFields @'["txInfo", "purpose"] ctx'
  txInfo <- pletFields @'["inputs", "mint"] ctx.txInfo

  PMinting ownSymbol' <- pmatch ctx.purpose
  ownSymbol <- plet $ pfromData $ pfield @"_0" # ownSymbol'
  ownMint <- plet $ ptryGetOwnMint # ownSymbol # txInfo.mint
  PCons mintedNfts mintedRest <- pmatch (pto ownMint)

  let mintedNftName = pfromData $ pfstBuiltin # mintedNfts
  let mintedNftAmount = pfromData $ psndBuiltin # mintedNfts

  isInitialSpend <- plet $ plam $ \(txInInfo' :: Term s PTxInInfo) -> P.do
    txInInfo <- pletFields @'["outRef"] txInInfo'
    txInInfo.outRef #== initialSpend

  passert "Minted NFT more than one NFT" (mintedNftAmount #== 1)

  passert "Minted NFT name doesn't match initial spend" (mintedNftName #== expectedNftName)

  passert "Minted more than one token name" (pnull # mintedRest)

  passert "Initial spend was not spent" (pany # isInitialSpend # txInfo.inputs)

  popaque $ pconstant ()

pderiveNftName :: ClosedTerm (PTxOutRef :--> PTokenName)
pderiveNftName = phoistAcyclic $ plam $ \txOutRef' -> P.do
  txOutRef <- pletFields @'["id", "idx"] txOutRef'
  let txId = pfield @"_0" # txOutRef.id
      idxPart = pintegerToByteString # txOutRef.idx
  pcon $ PTokenName (txId <> idxPart)
