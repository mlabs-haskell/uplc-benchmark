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
  DexRedeemer (
    DexRedeemer'DepositLiquidity,
    DexRedeemer'Swap,
    DexRedeemer'WithdrawLiquidity
  ),
 )
import Plutarch (Script (Script))
import Plutarch.Test.Program (
  ScriptCase (ScriptCase),
  ScriptResult (ScriptFailure, ScriptSuccess),
  testScript,
 )
import Plutus.ContextBuilder (
  UTXO,
  buildSpending',
  input,
  mint,
  output,
  withInlineDatum,
  withSpendingUTXO,
  withValue,
 )
import PlutusLedgerApi.V1.Value (
  AssetClass (AssetClass),
  assetClassValue,
  singleton,
 )
import PlutusLedgerApi.V2 (
  CurrencySymbol (CurrencySymbol),
  ScriptContext,
  TokenName,
 )
import Test.Tasty (TestTree, testGroup)
import UplcBenchmark.ScriptLoader (uncheckedApplyDataToScript)
import UplcBenchmark.Spec.ContextBuilder.Utils (mkHash28)

lpToken :: CurrencySymbol
lpToken = CurrencySymbol $ mkHash28 1

tokenA :: AssetClass
tokenA = AssetClass (CurrencySymbol $ mkHash28 2, "A")

tokenB :: AssetClass
tokenB = AssetClass (CurrencySymbol $ mkHash28 3, "B")

poolNftName :: TokenName
poolNftName = "NFT"

poolNft :: AssetClass
poolNft = AssetClass (CurrencySymbol $ mkHash28 4, poolNftName)

poolDatum :: DexDatum
poolDatum =
  DexDatum
    { dexDatum'tokenA = tokenA
    , dexDatum'tokenB = tokenB
    , dexDatum'poolNft = poolNft
    , dexDatum'lpToken = lpToken
    , dexDatum'mintedLpTokens = 1415
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

validDeposit :: ScriptContext
validDeposit =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , mint $ singleton lpToken poolNftName 140
      , output $
          mconcat
            [ withInlineDatum $ poolDatum {dexDatum'mintedLpTokens = 1415 + 140}
            , withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 1100
            , withValue $ assetClassValue tokenB 2200
            ]
      ]

invalidDepositNoDeposit :: ScriptContext
invalidDepositNoDeposit =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , mint $ singleton lpToken poolNftName 140
      , output $
          mconcat
            [ withInlineDatum $ poolDatum {dexDatum'mintedLpTokens = 1415 + 140}
            , withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 1000
            , withValue $ assetClassValue tokenB 2000
            ]
      ]

invalidDepositNotEnough :: ScriptContext
invalidDepositNotEnough =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , mint $ singleton lpToken poolNftName 140
      , output $
          mconcat
            [ withInlineDatum $ poolDatum {dexDatum'mintedLpTokens = 1415 + 140}
            , withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 1050
            , withValue $ assetClassValue tokenB 2100
            ]
      ]

invalidDepositStealNft :: ScriptContext
invalidDepositStealNft =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , mint $ singleton lpToken poolNftName 140
      , output $
          mconcat
            [ withInlineDatum $ poolDatum {dexDatum'mintedLpTokens = 1415 + 140}
            , withValue $ assetClassValue tokenA 1100
            , withValue $ assetClassValue tokenB 2200
            ]
      ]

invalidDepositNoDatum :: ScriptContext
invalidDepositNoDatum =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , mint $ singleton lpToken poolNftName 140
      , output $
          mconcat
            [ withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 1100
            , withValue $ assetClassValue tokenB 2200
            ]
      ]

invalidDepositModifiedDatum :: ScriptContext
invalidDepositModifiedDatum =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , mint $ singleton lpToken poolNftName 140
      , output $
          mconcat
            [ withInlineDatum $
                poolDatum
                  { dexDatum'mintedLpTokens = 1415 + 140
                  , dexDatum'tokenA = tokenB
                  }
            , withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 1100
            , withValue $ assetClassValue tokenB 2200
            ]
      ]

validWithdraw :: ScriptContext
validWithdraw =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , mint $ singleton lpToken poolNftName (-150)
      , output $
          mconcat
            [ withInlineDatum $ poolDatum {dexDatum'mintedLpTokens = 1415 - 150}
            , withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 900
            , withValue $ assetClassValue tokenB 1800
            ]
      ]

invalidWithdrawNoWithdraw :: ScriptContext
invalidWithdrawNoWithdraw =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , mint $ singleton lpToken poolNftName (-150)
      , output $
          mconcat
            [ withInlineDatum $ poolDatum {dexDatum'mintedLpTokens = 1415 - 150}
            , withValue $ assetClassValue poolNft 1
            ]
      ]

invalidWithdrawTooMuch :: ScriptContext
invalidWithdrawTooMuch =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , mint $ singleton lpToken poolNftName (-150)
      , output $
          mconcat
            [ withInlineDatum $ poolDatum {dexDatum'mintedLpTokens = 1415 - 150}
            , withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 500
            , withValue $ assetClassValue tokenB 1000
            ]
      ]

invalidWithdrawStealNft :: ScriptContext
invalidWithdrawStealNft =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , mint $ singleton lpToken poolNftName (-150)
      , output $
          mconcat
            [ withInlineDatum $ poolDatum {dexDatum'mintedLpTokens = 1415 - 150}
            , withValue $ assetClassValue tokenA 900
            , withValue $ assetClassValue tokenB 1800
            ]
      ]

invalidWithdrawNoDatum :: ScriptContext
invalidWithdrawNoDatum =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , mint $ singleton lpToken poolNftName (-150)
      , output $
          mconcat
            [ withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 900
            , withValue $ assetClassValue tokenB 1800
            ]
      ]

invalidWithdrawModifiedDatum :: ScriptContext
invalidWithdrawModifiedDatum =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , mint $ singleton lpToken poolNftName (-150)
      , output $
          mconcat
            [ withInlineDatum $
                poolDatum
                  { dexDatum'mintedLpTokens = 1415 - 150
                  , dexDatum'tokenA = tokenB
                  }
            , withValue $ assetClassValue poolNft 1
            , withValue $ assetClassValue tokenA 900
            , withValue $ assetClassValue tokenB 1800
            ]
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
        Script applied = apply script
       in
        testScript $ ScriptCase testName expectedResult applied applied

    mkSwapTest :: String -> ScriptContext -> ScriptResult -> TestTree
    mkSwapTest = mkTest DexRedeemer'Swap

    mkDepositTest :: String -> ScriptContext -> ScriptResult -> TestTree
    mkDepositTest = mkTest DexRedeemer'DepositLiquidity

    mkWithdrawTest :: String -> ScriptContext -> ScriptResult -> TestTree
    mkWithdrawTest = mkTest DexRedeemer'WithdrawLiquidity
   in
    testGroup
      "Pool Validator"
      [ testGroup
          "Swap"
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
      , testGroup
          "Deposit"
          [ mkDepositTest "Valid Deposit" validDeposit ScriptSuccess
          , mkDepositTest "Invalid Deposit - mint but no deposit" invalidDepositNoDeposit ScriptFailure
          , mkDepositTest "Invalid Deposit - not deposit enough" invalidDepositNotEnough ScriptFailure
          , mkDepositTest "Invalid Deposit - steal NFT" invalidDepositStealNft ScriptFailure
          , mkDepositTest "Invalid Deposit - no datum" invalidDepositNoDatum ScriptFailure
          , mkDepositTest "Invalid Deposit - modified datum" invalidDepositModifiedDatum ScriptFailure
          ]
      , testGroup
          "Withdraw"
          [ mkWithdrawTest "Valid Withdraw" validWithdraw ScriptSuccess
          , mkWithdrawTest "Invalid Withdraw - mint but no withdraw" invalidWithdrawNoWithdraw ScriptFailure
          , mkWithdrawTest "Invalid Withdraw - withdraw too much" invalidWithdrawTooMuch ScriptFailure
          , mkWithdrawTest "Invalid Withdraw - steal NFT" invalidWithdrawStealNft ScriptFailure
          , mkWithdrawTest "Invalid Withdraw - no datum" invalidWithdrawNoDatum ScriptFailure
          , mkWithdrawTest "Invalid Withdraw - modified datum" invalidWithdrawModifiedDatum ScriptFailure
          ]
      ]
