module UplcBenchmark.Spec.PoolValidator (specForScript) where

import LambdaBuffers.Dex (
  DexDatum (
    DexDatum,
    dexDatum'lpToken,
    dexDatum'mintedLpTokens,
    dexDatum'poolNft,
    dexDatum'swapFee,
    dexDatum'tokenA,
    dexDatum'tokenB
  ),
  DexRedeemer (DexRedeemer'Swap),
 )
import Plutarch (Script)
import Plutarch.Context (
  UTXO,
  buildSpending',
  input,
  output,
  withSpendingUTXO,
  withValue,
 )
import Plutarch.Test.Script (
  ScriptCase (ScriptCase),
  ScriptResult (ScriptFailure, ScriptSuccess),
  testScript,
 )
import PlutusLedgerApi.V1.Value (AssetClass (AssetClass), assetClassValue)
import PlutusLedgerApi.V2 (
  CurrencySymbol (CurrencySymbol),
  ScriptContext,
 )
import Test.Tasty (TestTree, testGroup)
import UplcBenchmark.ScriptLoader (uncheckedApplyDataToScript)
import UplcBenchmark.Spec.ContextBuilder.Utils (
  mkHash28,
  withInlineDatum,
 )

lpToken :: CurrencySymbol
lpToken = CurrencySymbol $ mkHash28 1

tokenA :: AssetClass
tokenA = AssetClass (CurrencySymbol $ mkHash28 2, "A")

tokenB :: AssetClass
tokenB = AssetClass (CurrencySymbol $ mkHash28 3, "B")

poolNft :: AssetClass
poolNft = AssetClass (CurrencySymbol $ mkHash28 4, "NFT")

poolDatum :: DexDatum
poolDatum =
  DexDatum
    { dexDatum'tokenA = tokenA
    , dexDatum'tokenB = tokenB
    , dexDatum'poolNft = poolNft
    , dexDatum'lpToken = lpToken
    , dexDatum'mintedLpTokens = 1000
    , dexDatum'swapFee = 3
    }

poolUTxO :: UTXO
poolUTxO =
  mconcat
    [ withInlineDatum poolDatum
    , withValue $ assetClassValue poolNft 1
    , withValue $ assetClassValue tokenA 1000
    , withValue $ assetClassValue tokenB 2000
    ]

validSwapAForB :: ScriptContext
validSwapAForB =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , output $
          mconcat
            [ withInlineDatum poolDatum
            , withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 500
            , withValue $ assetClassValue tokenB 4007
            ]
      ]

validSwapBForA :: ScriptContext
validSwapBForA =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , output $
          mconcat
            [ withInlineDatum poolDatum
            , withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 2004
            , withValue $ assetClassValue tokenB 1000
            ]
      ]

invalidSwapNoFee :: ScriptContext
invalidSwapNoFee =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , output $
          mconcat
            [ withInlineDatum poolDatum
            , withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 500
            , withValue $ assetClassValue tokenB 4000
            ]
      ]

invalidSwapStealNft :: ScriptContext
invalidSwapStealNft =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , output $
          mconcat
            [ withInlineDatum poolDatum
            , withValue $ assetClassValue tokenA 500
            , withValue $ assetClassValue tokenB 4007
            ]
      ]

invalidSwapNoDatum :: ScriptContext
invalidSwapNoDatum =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , output $
          mconcat
            [ withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 500
            , withValue $ assetClassValue tokenB 4007
            ]
      ]

invalidSwapModifiedDatum :: ScriptContext
invalidSwapModifiedDatum =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , output $
          mconcat
            [ withInlineDatum $ poolDatum {dexDatum'mintedLpTokens = 42}
            , withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 500
            , withValue $ assetClassValue tokenB 4007
            ]
      ]

invalidSwapUnitDatum :: ScriptContext
invalidSwapUnitDatum =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , output $
          mconcat
            [ withInlineDatum ()
            , withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 500
            , withValue $ assetClassValue tokenB 4007
            ]
      ]

invalidSwapDrainPool :: ScriptContext
invalidSwapDrainPool =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , output $
          mconcat
            [ withInlineDatum poolDatum
            , withValue $ assetClassValue poolNft 1
            ]
      ]

invalidSwapNoSelfOutput :: ScriptContext
invalidSwapNoSelfOutput =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      ]

specForScript :: Script -> TestTree
specForScript script =
  let
    mkTest :: DexRedeemer -> String -> ScriptContext -> ScriptResult -> TestTree
    mkTest redeemer testName context expectedResult =
      let
        apply =
          uncheckedApplyDataToScript context
            . uncheckedApplyDataToScript redeemer
            . uncheckedApplyDataToScript poolDatum
        applied = apply script
       in
        testScript $ ScriptCase testName expectedResult applied applied

    mkSwapTest :: String -> ScriptContext -> ScriptResult -> TestTree
    mkSwapTest = mkTest DexRedeemer'Swap
   in
    testGroup
      "Pool Validator"
      [ mkSwapTest "Valid Swap - A for B" validSwapAForB ScriptSuccess
      , mkSwapTest "Valid Swap - B for A" validSwapBForA ScriptSuccess
      , mkSwapTest "Invalid Swap - no fee" invalidSwapNoFee ScriptFailure
      , mkSwapTest "Invalid Swap - steal NFT" invalidSwapStealNft ScriptFailure
      , mkSwapTest "Invalid Swap - no datum" invalidSwapNoDatum ScriptFailure
      , mkSwapTest "Invalid Swap - modified datum" invalidSwapModifiedDatum ScriptFailure
      , mkSwapTest "Invalid Swap - unit datum" invalidSwapUnitDatum ScriptFailure
      , mkSwapTest "Invalid Swap - drain pool" invalidSwapDrainPool ScriptFailure
      , mkSwapTest "Invalid Swap - no self output" invalidSwapNoSelfOutput ScriptFailure
      ]
