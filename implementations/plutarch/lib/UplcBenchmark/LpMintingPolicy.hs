module UplcBenchmark.LpMintingPolicy (plpMintingPolicy) where

import LambdaBuffers.Dex.Plutarch (
  LpMintingPolicyRedeemer (
    LpMintingPolicyRedeemer'CreatePool,
    LpMintingPolicyRedeemer'ForwardCheck
  ),
 )
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2 (PCurrencySymbol, PMintingPolicy, PTokenName, PTxInInfo)
import Plutarch.Monadic qualified as P

import UplcBenchmark.Utils (passert, ptryDecodeData)

plpMintingPolicy :: ClosedTerm (PAsData PCurrencySymbol :--> PAsData PTokenName :--> PMintingPolicy)
plpMintingPolicy = plam $ \poolNftCs poolNftTn rawRedeemer ctx' -> P.do
  redeemer <- plet $ ptryDecodeData @LpMintingPolicyRedeemer rawRedeemer
  ctx <- pletFields @'["txInfo"] ctx'
  pmatch redeemer $ \case
    LpMintingPolicyRedeemer'CreatePool -> P.do
      txInfo <- pletFields @'["mint"] ctx.txInfo

      passert
        "Must mint one Pool NFT token"
        (pvalueOf # txInfo.mint # pfromData poolNftCs # pfromData poolNftTn #== 1)

      popaque $ pconstant ()
    LpMintingPolicyRedeemer'ForwardCheck -> P.do
      txInfo <- pletFields @'["inputs"] ctx.txInfo

      inputHasNft :: Term s (PTxInInfo :--> PBool) <- plet $ plam $ \txInInfo' -> P.do
        txInInfo <- pletFields @'["resolved"] txInInfo'
        txOut <- pletFields @'["value"] txInInfo.resolved
        pvalueOf # txOut.value # pfromData poolNftCs # pfromData poolNftTn #== 1

      passert
        "Must spend one Pool NFT token"
        (pany # inputHasNft # txInfo.inputs)

      popaque $ pconstant ()
