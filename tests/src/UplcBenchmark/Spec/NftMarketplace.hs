module UplcBenchmark.Spec.NftMarketplace (specForScript) where

import Data.ByteString qualified as ByteString
import Data.ByteString.Hash (blake2b_256)
import Data.Word (Word8)
import LambdaBuffers.NftMarketplace (
  NftMarketplaceDatum (NftMarketplaceDatum),
  NftMarketplaceRedeemer (NftMarketplaceRedeemer'Buy),
 )
import Optics (set)
import Plutarch (Script)
import Plutarch.Context (
  UTXO,
  address,
  buildSpending',
  input,
  output,
  withRef,
  withSpendingUTXO,
  withValue,
 )
import Plutarch.Context.Base (DatumType (InlineDatum))
import Plutarch.Test.Script (
  ScriptCase (ScriptCase),
  ScriptResult (ScriptFailure, ScriptSuccess),
  testScript,
 )
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V2 (
  Address,
  BuiltinByteString,
  PubKeyHash (PubKeyHash),
  ScriptContext,
  ToData,
  TxId (TxId),
  TxOutRef (TxOutRef),
  adaSymbol,
  adaToken,
  singleton,
  toBuiltin,
  toData,
 )
import Test.Tasty (TestTree, testGroup)
import UplcBenchmark.ScriptLoader (uncheckedApplyDataToScript)

mkHash32 :: Word8 -> BuiltinByteString
mkHash32 idx = toBuiltin $ blake2b_256 $ ByteString.pack [idx]

mkHash28 :: Word8 -> BuiltinByteString
mkHash28 idx = toBuiltin $ ByteString.take 28 $ blake2b_256 $ ByteString.pack [idx]

mkTxId :: Word8 -> TxId
mkTxId = TxId . mkHash32

withRedeemer' :: (ToData redeemer) => redeemer -> UTXO
withRedeemer' r = set #redeemer (Just $ toData r) (mempty :: UTXO)

withInlineDatum' :: (ToData datum) => datum -> UTXO
withInlineDatum' dat = set #data (pure . InlineDatum . toData $ dat) (mempty :: UTXO)

validatedUTxORef :: TxOutRef
validatedUTxORef = TxOutRef (mkTxId 0) 42

validatedOrderUTxO :: UTXO
validatedOrderUTxO =
  mconcat
    [ withRef validatedUTxORef
    , withRedeemer' NftMarketplaceRedeemer'Buy
    ]

validPaymentUTxO :: UTXO
validPaymentUTxO =
  mconcat
    [ withValue (singleton adaSymbol adaToken 100_000_000)
    , address validSellerAddress
    , withInlineDatum' validatedUTxORef
    ]

invalidPaymentUTxONoDatum :: UTXO
invalidPaymentUTxONoDatum =
  mconcat
    [ withValue (singleton adaSymbol adaToken 100_000_000)
    , address validSellerAddress
    ]

validSellerAddress :: Address
validSellerAddress = pubKeyHashAddress validCancelPubKeyHash

validCancelPubKeyHash :: PubKeyHash
validCancelPubKeyHash = PubKeyHash $ mkHash28 1

validatedOrderDatum :: NftMarketplaceDatum
validatedOrderDatum =
  NftMarketplaceDatum
    (singleton adaSymbol adaToken 100_000_000)
    validSellerAddress
    validCancelPubKeyHash

validSpendOne :: ScriptContext
validSpendOne =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , output validPaymentUTxO
      ]

invalidOneNoDatum :: ScriptContext
invalidOneNoDatum =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , output invalidPaymentUTxONoDatum
      ]

specForScript :: Script -> TestTree
specForScript script =
  let
    mkTest :: String -> ScriptContext -> ScriptResult -> TestTree
    mkTest testName context expectedResult =
      let
        apply =
          uncheckedApplyDataToScript context
            . uncheckedApplyDataToScript NftMarketplaceRedeemer'Buy
            . uncheckedApplyDataToScript validatedOrderDatum
        applied = apply script
       in
        testScript $ ScriptCase testName expectedResult applied applied
   in
    testGroup
      "NFT Marketplace"
      [ mkTest "Valid one spend" validSpendOne ScriptSuccess
      , mkTest "Invalid one spend - no datum" invalidOneNoDatum ScriptFailure
      ]
