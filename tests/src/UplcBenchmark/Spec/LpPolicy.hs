module UplcBenchmark.Spec.LpPolicy (specForScript, mkValidMintOneNft, mkValidMintTwoNfts) where

import Plutarch.Script (Script (Script))
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
  scriptRedeemer,
  withMinting,
  withValue,
 )
import PlutusLedgerApi.V3 (
  CurrencySymbol (CurrencySymbol),
  Redeemer (Redeemer),
  ScriptContext,
  ToData (toBuiltinData),
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

unitRedeemer :: Redeemer
unitRedeemer = Redeemer $ toBuiltinData ()

withLpMinting :: CurrencySymbol -> [MintingBuilder] -> Redeemer -> ScriptContext
withLpMinting ownSymbol builder redeemer =
  buildMinting' $ mconcat builder <> scriptRedeemer redeemer <> withMinting ownSymbol

validMint :: CurrencySymbol -> Redeemer -> ScriptContext
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

validMintTwoNfts :: CurrencySymbol -> Redeemer -> ScriptContext
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

invalidMintNoNft :: CurrencySymbol -> Redeemer -> ScriptContext
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

invalidMintOneOfTwoNfts :: CurrencySymbol -> Redeemer -> ScriptContext
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

invalidMintWrongName :: CurrencySymbol -> Redeemer -> ScriptContext
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

mkTest :: String -> (CurrencySymbol -> Redeemer -> ScriptContext) -> ScriptResult -> Script -> ScriptCase
mkTest testName context expectedResult script =
  let
    withParameter = uncheckedApplyDataToScript poolNftSymbol script

    ownSymbol = scriptSymbol withParameter

    apply = uncheckedApplyDataToScript (context ownSymbol unitRedeemer)

    Script applied = apply withParameter
   in
    ScriptCase testName expectedResult applied applied

mkValidMintOneNft :: Script -> ScriptCase
mkValidMintOneNft = mkTest "Valid mint - one NFT" validMint ScriptSuccess

mkValidMintTwoNfts :: Script -> ScriptCase
mkValidMintTwoNfts = mkTest "Valid mint - two NFTs" validMintTwoNfts ScriptSuccess

specForScript :: Script -> TestTree
specForScript script =
  let
   in testGroup
        "LP Minting Policy"
        $ fmap
          (testScript . ($ script))
          [ mkValidMintOneNft
          , mkValidMintTwoNfts
          , mkTest "Invalid mint - don't spend NFT" invalidMintNoNft ScriptFailure
          , mkTest "Invalid mint - one of two NFTs" invalidMintOneOfTwoNfts ScriptFailure
          , mkTest "Invalid mint - wrong NFT name" invalidMintWrongName ScriptFailure
          ]
