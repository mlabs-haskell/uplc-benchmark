module UplcBenchmark.Spec.PoolValidator (specForScript, mkWithdraw, mkDeposit, mkSwapBForA, mkSwapAForB) where

import Data.Kind (Type)
import Plutarch.Script (Script (Script))
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
  scriptRedeemer,
  withInlineDatum,
  withSpendingUTXO,
  withValue,
 )
import PlutusLedgerApi.V1.Value (
  AssetClass (AssetClass),
  assetClassValue,
  singleton,
 )
import PlutusLedgerApi.V3 (
  CurrencySymbol (CurrencySymbol),
  Redeemer (Redeemer),
  ScriptContext,
  ToData (toBuiltinData),
  TokenName (TokenName),
 )
import Test.Tasty (TestTree, testGroup)

import UplcBenchmark.ScriptLoader (uncheckedApplyDataToScript)
import UplcBenchmark.Spec.ContextBuilder.Utils (mkHash28)

type DexDatum :: Type
data DexDatum = DexDatum
  { dexDatum'tokenA :: AssetClass
  , dexDatum'tokenB :: AssetClass
  , dexDatum'poolNft :: AssetClass
  , dexDatum'lpToken :: CurrencySymbol
  , dexDatum'mintedLpTokens :: Integer
  , dexDatum'swapFee :: Integer
  }

instance ToData DexDatum where
  toBuiltinData (DexDatum tokenA' tokenB' poolNft' lpToken' mintedLpTokens swapFee) =
    toBuiltinData
      [ toBuiltinData tokenA'
      , toBuiltinData tokenB'
      , toBuiltinData poolNft'
      , toBuiltinData lpToken'
      , toBuiltinData mintedLpTokens
      , toBuiltinData swapFee
      ]

type DexRedeemer :: Type
data DexRedeemer
  = DexRedeemer'Swap
  | DexRedeemer'DepositLiquidity
  | DexRedeemer'WithdrawLiquidity

instance ToData DexRedeemer where
  toBuiltinData DexRedeemer'Swap = toBuiltinData (0 :: Integer)
  toBuiltinData DexRedeemer'DepositLiquidity = toBuiltinData (1 :: Integer)
  toBuiltinData DexRedeemer'WithdrawLiquidity = toBuiltinData (2 :: Integer)

lpToken :: CurrencySymbol
lpToken = CurrencySymbol $ mkHash28 1

tokenA :: AssetClass
tokenA = AssetClass (CurrencySymbol $ mkHash28 2, TokenName "A")

tokenB :: AssetClass
tokenB = AssetClass (CurrencySymbol $ mkHash28 3, TokenName "B")

poolNftName :: TokenName
poolNftName = TokenName "NFT"

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

validSwapAForB :: Redeemer -> ScriptContext
validSwapAForB redeemer =
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
      , scriptRedeemer redeemer
      ]

validSwapBForA :: Redeemer -> ScriptContext
validSwapBForA redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidSwapNoFee :: Redeemer -> ScriptContext
invalidSwapNoFee redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidSwapStealNft :: Redeemer -> ScriptContext
invalidSwapStealNft redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidSwapNoDatum :: Redeemer -> ScriptContext
invalidSwapNoDatum redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidSwapModifiedDatum :: Redeemer -> ScriptContext
invalidSwapModifiedDatum redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidSwapUnitDatum :: Redeemer -> ScriptContext
invalidSwapUnitDatum redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidSwapDrainPool :: Redeemer -> ScriptContext
invalidSwapDrainPool redeemer =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , output $
          mconcat
            [ withInlineDatum poolDatum
            , withValue $ assetClassValue poolNft 1
            ]
      , scriptRedeemer redeemer
      ]

invalidSwapNoSelfOutput :: Redeemer -> ScriptContext
invalidSwapNoSelfOutput redeemer =
  buildSpending' $
    mconcat
      [ input poolUTxO
      , withSpendingUTXO poolUTxO
      , scriptRedeemer redeemer
      ]

validDeposit :: Redeemer -> ScriptContext
validDeposit redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidDepositNoDeposit :: Redeemer -> ScriptContext
invalidDepositNoDeposit redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidDepositNotEnough :: Redeemer -> ScriptContext
invalidDepositNotEnough redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidDepositStealNft :: Redeemer -> ScriptContext
invalidDepositStealNft redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidDepositNoDatum :: Redeemer -> ScriptContext
invalidDepositNoDatum redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidDepositModifiedDatum :: Redeemer -> ScriptContext
invalidDepositModifiedDatum redeemer =
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
      , scriptRedeemer redeemer
      ]

validWithdraw :: Redeemer -> ScriptContext
validWithdraw redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidWithdrawNoWithdraw :: Redeemer -> ScriptContext
invalidWithdrawNoWithdraw redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidWithdrawTooMuch :: Redeemer -> ScriptContext
invalidWithdrawTooMuch redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidWithdrawStealNft :: Redeemer -> ScriptContext
invalidWithdrawStealNft redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidWithdrawNoDatum :: Redeemer -> ScriptContext
invalidWithdrawNoDatum redeemer =
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
      , scriptRedeemer redeemer
      ]

invalidWithdrawModifiedDatum :: Redeemer -> ScriptContext
invalidWithdrawModifiedDatum redeemer =
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
      , scriptRedeemer redeemer
      ]

mkTest :: DexRedeemer -> String -> (Redeemer -> ScriptContext) -> ScriptResult -> Script -> ScriptCase
mkTest redeemer testName context expectedResult script =
  let
    apply = uncheckedApplyDataToScript (context $ Redeemer $ toBuiltinData redeemer)
    Script applied = apply script
   in
    ScriptCase testName expectedResult applied applied

mkSwapTest :: String -> (Redeemer -> ScriptContext) -> ScriptResult -> Script -> ScriptCase
mkSwapTest = mkTest DexRedeemer'Swap

mkDepositTest :: String -> (Redeemer -> ScriptContext) -> ScriptResult -> Script -> ScriptCase
mkDepositTest = mkTest DexRedeemer'DepositLiquidity

mkWithdrawTest :: String -> (Redeemer -> ScriptContext) -> ScriptResult -> Script -> ScriptCase
mkWithdrawTest = mkTest DexRedeemer'WithdrawLiquidity

mkSwapAForB :: Script -> ScriptCase
mkSwapAForB = mkSwapTest "Valid Swap - A for B" validSwapAForB ScriptSuccess

mkSwapBForA :: Script -> ScriptCase
mkSwapBForA = mkSwapTest "Valid Swap - B for A" validSwapBForA ScriptSuccess

mkDeposit :: Script -> ScriptCase
mkDeposit = mkDepositTest "Valid Deposit" validDeposit ScriptSuccess

mkWithdraw :: Script -> ScriptCase
mkWithdraw = mkWithdrawTest "Valid Withdraw" validWithdraw ScriptSuccess

specForScript :: Script -> TestTree
specForScript script =
  testGroup
    "Pool Validator"
    [ testGroup
        "Swap"
        $ fmap
          (testScript . ($ script))
          [ mkSwapAForB
          , mkSwapBForA
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
        $ fmap
          (testScript . ($ script))
          [ mkDeposit
          , mkDepositTest "Invalid Deposit - mint but no deposit" invalidDepositNoDeposit ScriptFailure
          , mkDepositTest "Invalid Deposit - not deposit enough" invalidDepositNotEnough ScriptFailure
          , mkDepositTest "Invalid Deposit - steal NFT" invalidDepositStealNft ScriptFailure
          , mkDepositTest "Invalid Deposit - no datum" invalidDepositNoDatum ScriptFailure
          , mkDepositTest "Invalid Deposit - modified datum" invalidDepositModifiedDatum ScriptFailure
          ]
    , testGroup
        "Withdraw"
        $ fmap
          (testScript . ($ script))
          [ mkWithdraw
          , mkWithdrawTest "Invalid Withdraw - mint but no withdraw" invalidWithdrawNoWithdraw ScriptFailure
          , mkWithdrawTest "Invalid Withdraw - withdraw too much" invalidWithdrawTooMuch ScriptFailure
          , mkWithdrawTest "Invalid Withdraw - steal NFT" invalidWithdrawStealNft ScriptFailure
          , mkWithdrawTest "Invalid Withdraw - no datum" invalidWithdrawNoDatum ScriptFailure
          , mkWithdrawTest "Invalid Withdraw - modified datum" invalidWithdrawModifiedDatum ScriptFailure
          ]
    ]
