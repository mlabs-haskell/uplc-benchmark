module UplcBenchmark.Spec.LpPolicy (specForScript) where

import Plutarch (Script (Script))
import Plutarch.Test.Program (
  ScriptCase (ScriptCase),
  ScriptResult (ScriptFailure, ScriptSuccess),
  testScript,
 )
import Plutus.ContextBuilder (
  MintingBuilder,
  buildMinting',
  input,
  mintSingletonWith,
  withMinting,
  withValue,
 )
import PlutusLedgerApi.V2 (
  CurrencySymbol (CurrencySymbol),
  ScriptContext,
  TokenName (TokenName),
  adaSymbol,
  adaToken,
  singleton,
 )
import Test.Tasty (TestTree, testGroup)
import UplcBenchmark.ScriptLoader (uncheckedApplyDataToScript)
import UplcBenchmark.Spec.ContextBuilder.Utils (mkHash28, scriptSymbol)

poolNftSymbol :: CurrencySymbol
poolNftSymbol = CurrencySymbol $ mkHash28 1

poolNftName1 :: TokenName
poolNftName1 = TokenName "pool nft name 1"

poolNftName2 :: TokenName
poolNftName2 = TokenName "pool nft name 2"

unitRedeemer :: ()
unitRedeemer = ()

withLpMinting :: CurrencySymbol -> [MintingBuilder] -> ScriptContext
withLpMinting ownSymbol builder =
  buildMinting' $ mconcat builder <> withMinting ownSymbol

validMint :: CurrencySymbol -> ScriptContext
validMint ownSymbol =
  withLpMinting
    ownSymbol
    [ mintSingletonWith unitRedeemer ownSymbol poolNftName1 42
    , input $
        mconcat
          [ withValue $
              singleton adaSymbol adaToken 100_000_000
                <> singleton poolNftSymbol poolNftName1 1
          ]
    ]

validMintTwoNfts :: CurrencySymbol -> ScriptContext
validMintTwoNfts ownSymbol =
  withLpMinting
    ownSymbol
    [ mintSingletonWith unitRedeemer ownSymbol poolNftName1 42
    , mintSingletonWith unitRedeemer ownSymbol poolNftName2 42
    , input $
        mconcat
          [ withValue $
              singleton adaSymbol adaToken 100_000_000
                <> singleton poolNftSymbol poolNftName1 1
                <> singleton poolNftSymbol poolNftName2 1
          ]
    ]

invalidMintNoNft :: CurrencySymbol -> ScriptContext
invalidMintNoNft ownSymbol =
  withLpMinting
    ownSymbol
    [ mintSingletonWith unitRedeemer ownSymbol poolNftName1 42
    , input $
        mconcat
          [ withValue $
              singleton adaSymbol adaToken 100_000_000
          ]
    ]

invalidMintOneOfTwoNfts :: CurrencySymbol -> ScriptContext
invalidMintOneOfTwoNfts ownSymbol =
  withLpMinting
    ownSymbol
    [ mintSingletonWith unitRedeemer ownSymbol poolNftName1 42
    , mintSingletonWith unitRedeemer ownSymbol poolNftName2 42
    , input $
        mconcat
          [ withValue $
              singleton adaSymbol adaToken 100_000_000
                <> singleton poolNftSymbol poolNftName1 1
          ]
    ]

invalidMintWrongName :: CurrencySymbol -> ScriptContext
invalidMintWrongName ownSymbol =
  withLpMinting
    ownSymbol
    [ mintSingletonWith unitRedeemer ownSymbol poolNftName1 42
    , input $
        mconcat
          [ withValue $
              singleton adaSymbol adaToken 100_000_000
                <> singleton poolNftSymbol poolNftName2 1
          ]
    ]

specForScript :: Script -> TestTree
specForScript script =
  let
    mkTest :: String -> (CurrencySymbol -> ScriptContext) -> ScriptResult -> TestTree
    mkTest testName context expectedResult =
      let
        withParameter = uncheckedApplyDataToScript poolNftSymbol script

        ownSymbol = scriptSymbol withParameter

        apply =
          uncheckedApplyDataToScript (context ownSymbol)
            . uncheckedApplyDataToScript unitRedeemer

        Script applied = apply withParameter
       in
        testScript $ ScriptCase testName expectedResult applied applied
   in
    testGroup
      "LP Minting Policy"
      [ mkTest "Valid mint - one NFT" validMint ScriptSuccess
      , mkTest "Valid mint - two NFTs" validMintTwoNfts ScriptSuccess
      , mkTest "Invalid mint - don't spend NFT" invalidMintNoNft ScriptFailure
      , mkTest "Invalid mint - one of two NFTs" invalidMintOneOfTwoNfts ScriptFailure
      , mkTest "Invalid mint - wrong NFT name" invalidMintWrongName ScriptFailure
      ]
