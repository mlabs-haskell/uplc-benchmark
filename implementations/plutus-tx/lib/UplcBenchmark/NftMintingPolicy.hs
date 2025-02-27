{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module UplcBenchmark.NftMintingPolicy (nftMintingPolicy) where

import Data.Kind (Type)
import PlutusLedgerApi.V2 (
  BuiltinByteString,
  BuiltinData,
  FromData (fromBuiltinData),
  ScriptContext (scriptContextPurpose, scriptContextTxInfo),
  ScriptPurpose (Minting),
  TokenName (TokenName),
  TxId (getTxId),
  TxInInfo (txInInfoOutRef),
  TxInfo (txInfoInputs, txInfoMint),
  TxOutRef (txOutRefIdx),
  txOutRefId,
  unsafeFromBuiltinData,
 )
import PlutusTx.Prelude (
  BuiltinUnit,
  Integer,
  any,
  appendByteString,
  check,
  consByteString,
  divide,
  emptyByteString,
  modulo,
  traceIfFalse,
  ($),
  (&&),
  (<),
  (==),
 )
import UplcBenchmark.Utils (fromJustTrace, getOwnMint)

type NftMintingPolicyRedeemer :: Type
newtype NftMintingPolicyRedeemer = NftMintingPolicyRedeemer'CreatePool TxOutRef
  deriving newtype (FromData)

{-# INLINE integerToByteString #-}
integerToByteString :: Integer -> BuiltinByteString
integerToByteString = go emptyByteString
  where
    {-# INLINE go #-}
    go rest n =
      if n < 256
        then consByteString n rest
        else go (consByteString (n `modulo` 256) rest) (n `divide` 256)

{-# INLINE deriveNftName #-}
deriveNftName :: TxOutRef -> TokenName
deriveNftName txOutRef =
  let idPart = getTxId $ txOutRefId txOutRef
      idxPart = integerToByteString $ txOutRefIdx txOutRef
   in TokenName (appendByteString idPart idxPart)

{-# INLINE nftMintingPolicy #-}
nftMintingPolicy :: BuiltinData -> BuiltinData -> BuiltinUnit
nftMintingPolicy rawRedeemer rawCtx =
  let !ctx :: ScriptContext = unsafeFromBuiltinData rawCtx
      !redeemer :: NftMintingPolicyRedeemer =
        fromJustTrace "Could not decode redeemer" $ fromBuiltinData rawRedeemer

      NftMintingPolicyRedeemer'CreatePool !initialSpend = redeemer

      !expectedTokenName = deriveNftName initialSpend

      !txInfo = scriptContextTxInfo ctx
      !inputs = txInfoInputs txInfo
      !mint = txInfoMint txInfo

      Minting !ownSymbol = scriptContextPurpose ctx
      ![(!mintedNftName, !mintedNftAmount)] = getOwnMint ownSymbol mint

      isInitialSpend txInInfo = txInInfoOutRef txInInfo == initialSpend

      !mintingOneNft = traceIfFalse "Minted more than one NFT" (mintedNftAmount == 1)
      !mintingNftNameCorrect =
        traceIfFalse
          "Minted NFT name doesn't match initial spend"
          (mintedNftName == expectedTokenName)
      !spendingInitialSpend =
        traceIfFalse
          "Initial spend was not spent"
          (any isInitialSpend inputs)
   in check (mintingOneNft && mintingNftNameCorrect && spendingInitialSpend)
