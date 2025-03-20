module UplcBenchmark.NftMintingPolicy (pnftMintingPolicy) where

import Data.Kind (Type)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V3 (
  PScriptContext (PScriptContext),
  PScriptInfo (PMintingScript),
  PTokenName (PTokenName),
  PTxId (PTxId),
  PTxInInfo (PTxInInfo),
  PTxInfo (PTxInfo),
  PTxOutRef (PTxOutRef),
 )
import Plutarch.Monadic qualified as P

import UplcBenchmark.Utils (passert, pintegerToByteString, ptryGetOwnMint, punsafeCastRedeemer)

type NftMintingPolicyRedeemer :: S -> Type
newtype NftMintingPolicyRedeemer s = NftMintingPolicyRedeemer (Term s PTxOutRef)
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, PEq, PIsData)
  deriving (PlutusType) via DeriveNewtypePlutusType NftMintingPolicyRedeemer

pnftMintingPolicy :: ClosedTerm (PAsData PScriptContext :--> POpaque)
pnftMintingPolicy = plam $ \ctx' -> P.do
  PScriptContext txInfo redeemer info <- pmatch (pfromData ctx')
  NftMintingPolicyRedeemer initialSpend <- pmatch (pfromData $ punsafeCastRedeemer redeemer)
  expectedNftName <- plet $ pderiveNftName # initialSpend
  PTxInfo inputs _ _ _ mint _ _ _ _ _ _ _ _ _ _ _ <- pmatch txInfo
  PMintingScript ownSymbol <- pmatch info
  ownMint <- plet $ ptryGetOwnMint # pfromData ownSymbol # pfromData mint
  PCons mintedNfts mintedRest <- pmatch (pto ownMint)

  let mintedNftName = pfromData $ pfstBuiltin # mintedNfts
  let mintedNftAmount = pfromData $ psndBuiltin # mintedNfts

  isInitialSpend <- plet $ plam $ \txInInfo -> P.do
    PTxInInfo outRef _ <- pmatch (pfromData txInInfo)
    outRef #== initialSpend

  passert "Minted NFT more than one NFT" (mintedNftAmount #== 1)

  passert "Minted NFT name doesn't match initial spend" (mintedNftName #== expectedNftName)

  passert "Minted more than one token name" (pnull # mintedRest)

  passert "Initial spend was not spent" (pany # isInitialSpend # pfromData inputs)

  popaque $ pconstant @PUnit ()

pderiveNftName :: ClosedTerm (PTxOutRef :--> PTokenName)
pderiveNftName = phoistAcyclic $ plam $ \txOutRef -> P.do
  PTxOutRef txId' idx <- pmatch txOutRef
  PTxId txId <- pmatch $ pfromData txId'
  let idxPart = pintegerToByteString # pfromData idx
  pcon $ PTokenName (txId <> idxPart)
