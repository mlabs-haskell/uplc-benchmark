module UplcBenchmark.PoolValidator (ppoolValidator) where

import Data.Kind (Type)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V2 (
  POutputDatum (POutputDatum),
  PScriptContext (PScriptContext),
  PScriptPurpose (PSpending),
  PTxInInfo (PTxInInfo),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
 )
import Plutarch.LedgerApi.Value (PAssetClass (PAssetClass), PCurrencySymbol, pvalueOf)
import Plutarch.Monadic qualified as P
import Plutarch.Repr.Data (DeriveAsDataRec (DeriveAsDataRec))
import Plutarch.Repr.Tag (DeriveAsTag (DeriveAsTag))
import Plutarch.Unsafe (punsafeCoerce)
import UplcBenchmark.Utils (
  passert,
  pisPDNothing,
  ptryFindInput,
  ptryFindOutputWithAsset,
  pvalueOfAssetClass,
 )

type DexDatum :: S -> Type
data DexDatum s = DexDatum
  { tokenA :: Term s (PAsData PAssetClass)
  , tokenB :: Term s (PAsData PAssetClass)
  , poolNft :: Term s (PAsData PAssetClass)
  , lpToken :: Term s (PAsData PCurrencySymbol)
  , mintedLpTokens :: Term s (PAsData PInteger)
  , swapFee :: Term s (PAsData PInteger)
  }
  deriving stock (GHC.Generic)
  deriving anyclass (SOP.Generic, PEq, PIsData)
  deriving (PlutusType) via (DeriveAsDataRec DexDatum)

type DexRedeemer :: S -> Type
data DexRedeemer s
  = DexRedeemer'Swap
  | DexRedeemer'DepositLiquidity
  | DexRedeemer'WithdrawLiquidity
  deriving stock (GHC.Generic, Show)
  deriving anyclass (SOP.Generic, PEq, PIsData)
  deriving (PlutusType, PLiftable) via DeriveAsTag DexRedeemer

-- TODO: PTryFrom

-- FIXME: compile does not halt
ppoolValidator :: ClosedTerm (PAsData DexDatum :--> PAsData DexRedeemer :--> PAsData PScriptContext :--> POpaque)
ppoolValidator = plam $ \datum redeemer ctx' -> P.do
  DexDatum inTokenA inTokenB inPoolNft' inLpToken inMintedLpTokens inSwapFee <- pmatch (pfromData datum)
  inPoolNft <- plet $ pfromData inPoolNft'
  nftAndLpTokenName <- plet $ pmatch inPoolNft $ \(PAssetClass inPoolNft'') -> psndBuiltin # inPoolNft''
  PScriptContext txInfo purpose <- pmatch (pfromData ctx')
  PTxInfo inputs _ outputs _ mint _ _ _ _ _ _ _ <- pmatch txInfo
  PSpending ownTxOutRef <- pmatch purpose

  (PTxInInfo _ ownInputResolved) <- pmatch $ pfromData (ptryFindInput # ownTxOutRef # pfromData inputs)
  (PTxOut _ ownInputValue _ _) <- pmatch ownInputResolved

  (PTxOut _ ownOutputValue ownOutputDatum ownOutputReferenceScript) <- pmatch $ pfromData (ptryFindOutputWithAsset # inPoolNft # pfromData outputs)

  let hasNft = plam $ \value -> pvalueOfAssetClass # value # inPoolNft #== 1

  newMintedLp <- plet $ pvalueOf # pfromData mint # pfromData inLpToken # pfromData nftAndLpTokenName

  inAAmount <- plet $ pvalueOfAssetClass # pfromData ownInputValue # pfromData inTokenA
  inBAmount <- plet $ pvalueOfAssetClass # pfromData ownInputValue # pfromData inTokenB

  outAAmount <- plet $ pvalueOfAssetClass # pfromData ownOutputValue # pfromData inTokenA
  outBAmount <- plet $ pvalueOfAssetClass # pfromData ownOutputValue # pfromData inTokenB

  POutputDatum outputDatum' <- pmatch ownOutputDatum
  outputDatum <- plet $ pfromData $ punsafeCoerce @(PAsData DexDatum) @PData $ pto outputDatum'
  DexDatum outTokenA outTokenB outPoolNft outLpToken outMintedLpTokens outSwapFee <- pmatch outputDatum

  passert "Input UTxO must have NFT" (hasNft # pfromData ownInputValue)

  passert "Output cannot have ref script" (pisPDNothing # ownOutputReferenceScript)

  passert "Invalid output datum: tokenA" (inTokenA #== outTokenA)
  passert "Invalid output datum: tokenB" (inTokenB #== outTokenB)
  passert "Invalid output datum: poolNft" (pdata inPoolNft #== outPoolNft)
  passert "Invalid output datum: lpToken" (inLpToken #== outLpToken)
  passert "Invalid output datum: swapFee" (inSwapFee #== outSwapFee)

  pmatch (pfromData redeemer) $ \case
    -- NOTE: Can we just make these two checks be the same?
    DexRedeemer'DepositLiquidity -> P.do
      passert
        "Invalid output datum: mintedLpTokens"
        (pfromData inMintedLpTokens + newMintedLp #== pfromData outMintedLpTokens)

      passert "Must not burn LP tokens" (newMintedLp #> 0)

      passert
        "Not deposited enough"
        (pfromData outMintedLpTokens * pfromData outMintedLpTokens #<= outAAmount * outBAmount)

      popaque $ pconstant @PUnit ()
    DexRedeemer'WithdrawLiquidity -> P.do
      passert
        "Invalid output datum: mintedLpTokens"
        (pfromData inMintedLpTokens + newMintedLp #== pfromData outMintedLpTokens)

      passert "Must not mint LP tokens" (newMintedLp #< 0)

      passert
        "Withdrawn too much"
        (pfromData outMintedLpTokens * pfromData outMintedLpTokens #<= outAAmount * outBAmount)

      popaque $ pconstant @PUnit ()
    DexRedeemer'Swap -> P.do
      passert
        "Invalid output datum: mintedLpTokens"
        (inMintedLpTokens #== outMintedLpTokens)

      passert "Must not mint LP tokens" (newMintedLp #== 0)

      passert
        "Swapped too much"
        (pcheckSwap # pfromData inSwapFee # inAAmount # inBAmount # outAAmount # outBAmount)

      popaque $ pconstant @PUnit ()

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
