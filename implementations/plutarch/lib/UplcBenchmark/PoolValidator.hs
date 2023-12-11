module UplcBenchmark.PoolValidator (poolValidator) where

import LambdaBuffers.Dex.Plutarch (
  DexDatum (DexDatum),
  DexRedeemer (
    DexRedeemer'DepositLiquidity,
    DexRedeemer'Swap,
    DexRedeemer'WithdrawLiquidity
  ),
 )
import Plutarch.Api.V1.Value (pvalueOf)
import Plutarch.Api.V2 (POutputDatum (POutputDatum), PValidator)
import Plutarch.Api.V2.Contexts (PScriptPurpose (PSpending))
import Plutarch.Monadic qualified as P
import UplcBenchmark.Utils (
  passert,
  pisPDNothing,
  ptryDecodeData,
  ptryFindInput,
  ptryFindOutputWithAsset,
  pvalueOfAssetClass,
 )

poolValidator :: ClosedTerm PValidator
poolValidator = plam $ \rawDatum rawRedeemer ctx' -> P.do
  DexDatum inTokenA inTokenB inPoolNft inLpToken inMintedLpTokens inSwapFee <-
    pmatch $ ptryDecodeData @DexDatum rawDatum
  redeemer <- plet $ ptryDecodeData @DexRedeemer rawRedeemer

  nftAndLpTokenName <- plet $ pfield @"_1" # inPoolNft

  ctx <- pletFields @'["txInfo", "purpose"] ctx'
  txInfo <- pletFields @'["inputs", "outputs", "mint"] ctx.txInfo

  PSpending ((pfield @"_0" #) -> ownTxOutRef) <- pmatch ctx.purpose
  ownInput <-
    pletFields @'["value", "datum"] $
      pfield @"resolved"
        #$ ptryFindInput
        # ownTxOutRef
        # txInfo.inputs

  ownOutput <-
    pletFields @'["value", "datum", "referenceScript"] $
      ptryFindOutputWithAsset # pfromData inPoolNft # txInfo.outputs

  let hasNft = plam $ \value -> pvalueOfAssetClass # value # pfromData inPoolNft #== 1

  newMintedLp <- plet $ pvalueOf # txInfo.mint # pfromData inLpToken # nftAndLpTokenName

  inAAmount <- plet $ pvalueOfAssetClass # ownInput.value # pfromData inTokenA
  inBAmount <- plet $ pvalueOfAssetClass # ownInput.value # pfromData inTokenB

  outAAmount <- plet $ pvalueOfAssetClass # ownOutput.value # pfromData inTokenA
  outBAmount <- plet $ pvalueOfAssetClass # ownOutput.value # pfromData inTokenB

  POutputDatum outputDatum <- pmatch ownOutput.datum
  DexDatum outTokenA outTokenB outPoolNft outLpToken outMintedLpTokens outSwapFee <-
    pmatch $ ptryDecodeData @DexDatum $ pfromData $ pto $ pfield @"outputDatum" # outputDatum

  passert "Input UTxO must have NFT" (hasNft # ownInput.value)

  passert "Output cannot have ref script" (pisPDNothing # ownOutput.referenceScript)

  passert "Invalid output datum: tokenA" (inTokenA #== outTokenA)
  passert "Invalid output datum: tokenB" (inTokenB #== outTokenB)
  passert "Invalid output datum: poolNft" (inPoolNft #== outPoolNft)
  passert "Invalid output datum: lpToken" (inLpToken #== outLpToken)
  passert "Invalid output datum: swapFee" (inSwapFee #== outSwapFee)

  pmatch redeemer $ \case
    -- NOTE: Can we just make these two checks be the same?
    DexRedeemer'DepositLiquidity -> P.do
      passert
        "Invalid output datum: mintedLpTokens"
        (pfromData inMintedLpTokens + newMintedLp #== pfromData outMintedLpTokens)

      passert "Must not burn LP tokens" (newMintedLp #> 0)

      passert
        "Not deposited enough"
        (pfromData outMintedLpTokens * pfromData outMintedLpTokens #<= outAAmount * outBAmount)

      popaque $ pconstant ()
    DexRedeemer'WithdrawLiquidity -> P.do
      passert
        "Invalid output datum: mintedLpTokens"
        (pfromData inMintedLpTokens + newMintedLp #== pfromData outMintedLpTokens)

      passert "Must not mint LP tokens" (newMintedLp #< 0)

      passert
        "Withdrawn too much"
        (pfromData outMintedLpTokens * pfromData outMintedLpTokens #<= outAAmount * outBAmount)

      popaque $ pconstant ()
    DexRedeemer'Swap -> P.do
      passert
        "Invalid output datum: mintedLpTokens"
        (inMintedLpTokens #== outMintedLpTokens)

      passert "Must not mint LP tokens" (newMintedLp #== 0)

      passert
        "Swapped too much"
        (pcheckSwap # pfromData inSwapFee # inAAmount # inBAmount # outAAmount # outBAmount)

      popaque $ pconstant ()

pcheckSwap ::
  Term
    s
    ( PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PInteger
        :--> PBool
    )
pcheckSwap = plam $ \feeNum oldA oldB newA newB ->
  feeDen * feeDen * oldA * oldB #<= ((newA * feeDen) - ((getDiff # newA # oldA) * feeNum)) * ((newB * feeDen) - ((getDiff # newB # oldB) * feeNum))

-- The uniswap fee is 0.3%; here it is multiplied by 1000, so that the
-- on-chain code deals only in integers.
-- See: <https://uniswap.org/whitepaper.pdf> Eq (11) (Page 7.)
feeDen :: ClosedTerm PInteger
feeDen = 1000

getDiff :: Term s (PInteger :--> PInteger :--> PInteger)
getDiff = phoistAcyclic $
  plam $ \new old ->
    pif (new - old #<= 0) 0 (new - old)
