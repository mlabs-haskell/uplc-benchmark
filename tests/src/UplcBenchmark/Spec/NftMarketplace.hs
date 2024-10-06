module UplcBenchmark.Spec.NftMarketplace (specForScript, mkValidBuyOneTest) where

import LambdaBuffers.NftMarketplace (
  NftMarketplaceDatum (NftMarketplaceDatum),
  NftMarketplaceRedeemer (NftMarketplaceRedeemer'Buy, NftMarketplaceRedeemer'Cancel),
 )
import Plutarch (Script (Script))
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
  signedWith,
  withHashDatum,
  withInlineDatum,
  withRedeemer,
  withRef,
  withSpendingUTXO,
  withValue,
 )
import PlutusLedgerApi.V1.Address (pubKeyHashAddress)
import PlutusLedgerApi.V2 (
  Address,
  PubKeyHash (PubKeyHash),
  ScriptContext,
  TxId (TxId),
  TxOutRef (TxOutRef),
  adaSymbol,
  adaToken,
  singleton,
 )
import Test.Tasty (TestTree, testGroup)
import UplcBenchmark.ScriptLoader (uncheckedApplyDataToScript)
import UplcBenchmark.Spec.ContextBuilder.Utils (
  junkSymbol,
  junkToken,
  mkHash28,
  mkHash32,
 )

validatedUTxORef :: TxOutRef
validatedUTxORef = TxOutRef (TxId $ mkHash32 0) 42

validatedOrderUTxO :: UTXO
validatedOrderUTxO =
  mconcat
    [ withRef validatedUTxORef
    , withRedeemer NftMarketplaceRedeemer'Buy
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

validBuyOne :: ScriptContext
validBuyOne =
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

invalidOneHashDatum :: ScriptContext
invalidOneHashDatum =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , output invalidPaymentUTxOHashDatum
      ]

invalidOnePayTooLittle :: ScriptContext
invalidOnePayTooLittle =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , output invalidPaymentUTxOTooLittle
      ]

invalidOnePayTooMuch :: ScriptContext
invalidOnePayTooMuch =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , output invalidPaymentUTxOTooMuch
      ]

invalidOnePayWithJunk :: ScriptContext
invalidOnePayWithJunk =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , output invalidPaymentUTxOWithJunk
      ]

invalidNoPayment :: ScriptContext
invalidNoPayment =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      ]

validCancelOne :: ScriptContext
validCancelOne =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , signedWith validCancelPubKeyHash
      ]

invalidCancelOneWrongKey :: ScriptContext
invalidCancelOneWrongKey =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      , signedWith junkPubKeyHash
      ]

invalidCancelOneNoKey :: ScriptContext
invalidCancelOneNoKey =
  buildSpending' $
    mconcat
      [ input validatedOrderUTxO
      , withSpendingUTXO validatedOrderUTxO
      ]

mkTest :: NftMarketplaceRedeemer -> Script -> String -> ScriptContext -> ScriptResult -> ScriptCase
mkTest redeemer script testName context expectedResult =
  let
    apply =
      uncheckedApplyDataToScript context
        . uncheckedApplyDataToScript redeemer
        . uncheckedApplyDataToScript validatedOrderDatum
    Script applied = apply script
   in
    ScriptCase testName expectedResult applied applied

mkBuyTest :: Script -> String -> ScriptContext -> ScriptResult -> ScriptCase
mkBuyTest = mkTest NftMarketplaceRedeemer'Buy

mkCancelTest :: Script -> String -> ScriptContext -> ScriptResult -> ScriptCase
mkCancelTest = mkTest NftMarketplaceRedeemer'Cancel

mkValidBuyOneTest :: Script -> ScriptCase
mkValidBuyOneTest script = mkBuyTest script "Buy one" validBuyOne ScriptSuccess

specForScript :: Script -> TestTree
specForScript script =
  testGroup
    "NFT Marketplace"
    $ fmap
      testScript
      [ mkValidBuyOneTest script
      , mkBuyTest script "Invalid buy one - no datum" invalidOneNoDatum ScriptFailure
      , mkBuyTest script "Invalid buy one - hash datum" invalidOneHashDatum ScriptFailure
      , mkBuyTest script "Invalid buy one - pay too little" invalidOnePayTooLittle ScriptFailure
      , mkBuyTest script "Invalid buy one - pay too much" invalidOnePayTooMuch ScriptFailure
      , mkBuyTest script "Invalid buy one - pay with junk" invalidOnePayWithJunk ScriptFailure
      , mkBuyTest script "Invalid buy one - no payment" invalidNoPayment ScriptFailure
      , mkCancelTest script "Cancel one" validCancelOne ScriptSuccess
      , mkCancelTest script "Cancel one - wrong key" invalidCancelOneWrongKey ScriptFailure
      , mkCancelTest script "Cancel one - no key" invalidCancelOneNoKey ScriptFailure
      ]
