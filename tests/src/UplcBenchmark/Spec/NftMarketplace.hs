{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module UplcBenchmark.Spec.NftMarketplace (specForScript, mkValidBuyOneTest, mkCancelOneTest) where

import Data.Kind (Type)
import Plutarch.Script (Script (Script))
import Plutarch.Test.Program (
  ScriptCase (ScriptCase),
  ScriptResult (ScriptFailure, ScriptSuccess),
  testScript,
 )
import Plutus.ContextBuilder (
  UTXO,
  address,
  buildSpending',
  input,
  output,
  scriptRedeemer,
  signedWith,
  withHashDatum,
  withInlineDatum,
  withRedeemer,
  withRef,
  withSpendingUTXO,
  withValue,
 )
import PlutusLedgerApi.V3 (
  Address,
  PubKeyHash (PubKeyHash),
  Redeemer (Redeemer),
  ScriptContext,
  ToData (toBuiltinData),
  TxId (TxId),
  TxOutRef (TxOutRef),
  Value,
  adaSymbol,
  adaToken,
  singleton,
 )
import Test.Tasty (TestTree, testGroup)

import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import UplcBenchmark.ScriptLoader (uncheckedApplyDataToScript)
import UplcBenchmark.Spec.ContextBuilder.Utils (junkSymbol, junkToken, mkHash28, mkHash32)

type NftMarketplaceDatum :: Type
data NftMarketplaceDatum = NftMarketplaceDatum
  { price :: Value
  , seller :: Address
  , cancelKey :: PubKeyHash
  }

instance ToData NftMarketplaceDatum where
  toBuiltinData (NftMarketplaceDatum price seller cancelKey) =
    toBuiltinData [toBuiltinData price, toBuiltinData seller, toBuiltinData cancelKey]

type NftMarketplaceRedeemer :: Type
data NftMarketplaceRedeemer
  = NftMarketplaceRedeemer'Buy
  | NftMarketplaceRedeemer'Cancel

instance ToData NftMarketplaceRedeemer where
  toBuiltinData NftMarketplaceRedeemer'Buy = toBuiltinData (0 :: Integer)
  toBuiltinData NftMarketplaceRedeemer'Cancel = toBuiltinData (1 :: Integer)

validatedUTxORef :: TxOutRef
validatedUTxORef = TxOutRef (TxId $ mkHash32 0) 42

validatedOrderUTxO :: UTXO
validatedOrderUTxO =
  mconcat
    [ withRef validatedUTxORef
    , withRedeemer NftMarketplaceRedeemer'Buy
    , withInlineDatum validatedOrderDatum
    ]

validPaymentUTxO :: UTXO
validPaymentUTxO =
  mconcat
    [ withValue $ singleton adaSymbol adaToken 100_000_000
    , address validSellerAddress
    , withInlineDatum validatedUTxORef
    ]

invalidPaymentUTxONoDatum :: UTXO
invalidPaymentUTxONoDatum =
  mconcat
    [ withValue $ singleton adaSymbol adaToken 100_000_000
    , address validSellerAddress
    ]

invalidPaymentUTxOHashDatum :: UTXO
invalidPaymentUTxOHashDatum =
  mconcat
    [ withValue $ singleton adaSymbol adaToken 100_000_000
    , address validSellerAddress
    , withHashDatum validatedUTxORef
    ]

invalidPaymentUTxOTooLittle :: UTXO
invalidPaymentUTxOTooLittle =
  mconcat
    [ withValue $ singleton adaSymbol adaToken 90_000_000
    , address validSellerAddress
    , withInlineDatum validatedUTxORef
    ]

invalidPaymentUTxOTooMuch :: UTXO
invalidPaymentUTxOTooMuch =
  mconcat
    [ withValue $ singleton adaSymbol adaToken 110_000_000
    , address validSellerAddress
    , withInlineDatum validatedUTxORef
    ]

invalidPaymentUTxOWithJunk :: UTXO
invalidPaymentUTxOWithJunk =
  mconcat
    [ withValue $ singleton adaSymbol adaToken 100_000_000 <> singleton junkSymbol junkToken 42
    , address validSellerAddress
    , withInlineDatum validatedUTxORef
    ]

validSellerAddress :: Address
validSellerAddress = pubKeyHashAddress validCancelPubKeyHash

validCancelPubKeyHash :: PubKeyHash
validCancelPubKeyHash = PubKeyHash $ mkHash28 1

junkPubKeyHash :: PubKeyHash
junkPubKeyHash = PubKeyHash $ mkHash28 2

validatedOrderDatum :: NftMarketplaceDatum
validatedOrderDatum =
  NftMarketplaceDatum
    (singleton adaSymbol adaToken 100_000_000)
    validSellerAddress
    validCancelPubKeyHash

validBuyOne :: Redeemer -> ScriptContext
validBuyOne redeemer =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , output validPaymentUTxO
      , scriptRedeemer redeemer
      ]

invalidOneNoDatum :: Redeemer -> ScriptContext
invalidOneNoDatum redeemer =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , output invalidPaymentUTxONoDatum
      , scriptRedeemer redeemer
      ]

invalidOneHashDatum :: Redeemer -> ScriptContext
invalidOneHashDatum redeemer =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , output invalidPaymentUTxOHashDatum
      , scriptRedeemer redeemer
      ]

invalidOnePayTooLittle :: Redeemer -> ScriptContext
invalidOnePayTooLittle redeemer =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , output invalidPaymentUTxOTooLittle
      , scriptRedeemer redeemer
      ]

invalidOnePayTooMuch :: Redeemer -> ScriptContext
invalidOnePayTooMuch redeemer =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , output invalidPaymentUTxOTooMuch
      , scriptRedeemer redeemer
      ]

invalidOnePayWithJunk :: Redeemer -> ScriptContext
invalidOnePayWithJunk redeemer =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , output invalidPaymentUTxOWithJunk
      , scriptRedeemer redeemer
      ]

invalidNoPayment :: Redeemer -> ScriptContext
invalidNoPayment redeemer =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , scriptRedeemer redeemer
      ]

validCancelOne :: Redeemer -> ScriptContext
validCancelOne redeemer =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , signedWith validCancelPubKeyHash
      , scriptRedeemer redeemer
      ]

invalidCancelOneWrongKey :: Redeemer -> ScriptContext
invalidCancelOneWrongKey redeemer =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , signedWith junkPubKeyHash
      , scriptRedeemer redeemer
      ]

invalidCancelOneNoKey :: Redeemer -> ScriptContext
invalidCancelOneNoKey redeemer =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , scriptRedeemer redeemer
      ]

mkTest :: NftMarketplaceRedeemer -> String -> (Redeemer -> ScriptContext) -> ScriptResult -> Script -> ScriptCase
mkTest redeemer testName context expectedResult script =
  let
    apply = uncheckedApplyDataToScript (context $ Redeemer $ toBuiltinData redeemer)
    Script applied = apply script
   in
    ScriptCase testName expectedResult applied applied

mkBuyTest :: String -> (Redeemer -> ScriptContext) -> ScriptResult -> Script -> ScriptCase
mkBuyTest = mkTest NftMarketplaceRedeemer'Buy

mkCancelTest :: String -> (Redeemer -> ScriptContext) -> ScriptResult -> Script -> ScriptCase
mkCancelTest = mkTest NftMarketplaceRedeemer'Cancel

mkValidBuyOneTest :: Script -> ScriptCase
mkValidBuyOneTest = mkBuyTest "Buy one" validBuyOne ScriptSuccess

mkCancelOneTest :: Script -> ScriptCase
mkCancelOneTest = mkCancelTest "Cancel one" validCancelOne ScriptSuccess

specForScript :: Script -> TestTree
specForScript script =
  testGroup
    "NFT Marketplace"
    $ fmap
      (testScript . ($ script))
      [ mkValidBuyOneTest
      , mkBuyTest "Invalid buy one - no datum" invalidOneNoDatum ScriptFailure
      , mkBuyTest "Invalid buy one - hash datum" invalidOneHashDatum ScriptFailure
      , mkBuyTest "Invalid buy one - pay too little" invalidOnePayTooLittle ScriptFailure
      , mkBuyTest "Invalid buy one - pay too much" invalidOnePayTooMuch ScriptFailure
      , mkBuyTest "Invalid buy one - pay with junk" invalidOnePayWithJunk ScriptFailure
      , mkBuyTest "Invalid buy one - no payment" invalidNoPayment ScriptFailure
      , mkCancelOneTest
      , mkCancelTest "Cancel one - wrong key" invalidCancelOneWrongKey ScriptFailure
      , mkCancelTest "Cancel one - no key" invalidCancelOneNoKey ScriptFailure
      ]
