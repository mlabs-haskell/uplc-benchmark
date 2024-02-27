module UplcBenchmark.Spec.PoolNftPolicy (specForScript) where

import LambdaBuffers.Dex (NftMintingPolicyRedeemer (NftMintingPolicyRedeemer'CreatePool))
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
  withRef,
 )
import PlutusLedgerApi.V2 (
  BuiltinByteString,
  CurrencySymbol,
  ScriptContext,
  TokenName (TokenName),
  TxId (TxId),
  TxOutRef (TxOutRef),
 )
import PlutusTx.Builtins (consByteString, emptyByteString)
import Test.Tasty (TestTree, testGroup)
import UplcBenchmark.ScriptLoader (uncheckedApplyDataToScript)
import UplcBenchmark.Spec.ContextBuilder.Utils (mkHash32, scriptSymbol)

redeemer :: NftMintingPolicyRedeemer
redeemer = NftMintingPolicyRedeemer'CreatePool initialSpend

integerToByteString :: Integer -> BuiltinByteString
integerToByteString = go emptyByteString
  where
    go rest n
      | n < 256 = consByteString n rest
      | otherwise = go (consByteString (n `mod` 256) rest) (n `div` 256)

deriveNftName :: TxOutRef -> TokenName
deriveNftName (TxOutRef (TxId txId) idx) = TokenName (txId <> integerToByteString idx)

initialSpend :: TxOutRef
initialSpend = TxOutRef (TxId $ mkHash32 0) 42

withNftMinting :: CurrencySymbol -> [MintingBuilder] -> ScriptContext
withNftMinting ownSymbol builder =
  buildMinting' $ mconcat builder <> withMinting ownSymbol

validMint :: CurrencySymbol -> ScriptContext
validMint ownSymbol =
  withNftMinting
    ownSymbol
    [ mintSingletonWith redeemer ownSymbol (deriveNftName initialSpend) 1
    , input $
        mconcat
          [ withRef initialSpend
          ]
    ]

invalidMintMoreThanOne :: CurrencySymbol -> ScriptContext
invalidMintMoreThanOne ownSymbol =
  withNftMinting
    ownSymbol
    [ mintSingletonWith redeemer ownSymbol (deriveNftName initialSpend) 42
    , input $
        mconcat
          [ withRef initialSpend
          ]
    ]

invalidMintInvalidName :: CurrencySymbol -> ScriptContext
invalidMintInvalidName ownSymbol =
  withNftMinting
    ownSymbol
    [ mintSingletonWith redeemer ownSymbol "I'm invalid" 1
    , input $
        mconcat
          [ withRef initialSpend
          ]
    ]

invalidMintNoInitialSpend :: CurrencySymbol -> ScriptContext
invalidMintNoInitialSpend ownSymbol =
  withNftMinting
    ownSymbol
    [ mintSingletonWith redeemer ownSymbol (deriveNftName initialSpend) 1
    ]

invalidMintWrongId :: CurrencySymbol -> ScriptContext
invalidMintWrongId ownSymbol =
  withNftMinting
    ownSymbol
    [ mintSingletonWith redeemer ownSymbol (deriveNftName initialSpend) 1
    , input $
        mconcat
          [ withRef $ TxOutRef (TxId $ mkHash32 1) 42
          ]
    ]

invalidMintWrongIdx :: CurrencySymbol -> ScriptContext
invalidMintWrongIdx ownSymbol =
  withNftMinting
    ownSymbol
    [ mintSingletonWith redeemer ownSymbol (deriveNftName initialSpend) 1
    , input $
        mconcat
          [ withRef $ TxOutRef (TxId $ mkHash32 0) 123
          ]
    ]

invalidMintIdxTooHigh :: CurrencySymbol -> ScriptContext
invalidMintIdxTooHigh ownSymbol =
  let invalidInitialSpend = TxOutRef (TxId $ mkHash32 0) 256
   in withNftMinting
        ownSymbol
        [ mintSingletonWith redeemer ownSymbol (deriveNftName invalidInitialSpend) 1
        , input $
            mconcat
              [ withRef invalidInitialSpend
              ]
        ]

specForScript :: Script -> TestTree
specForScript script =
  let
    mkTest :: String -> (CurrencySymbol -> ScriptContext) -> ScriptResult -> TestTree
    mkTest testName context expectedResult =
      let
        ownSymbol = scriptSymbol script

        apply =
          uncheckedApplyDataToScript (context ownSymbol)
            . uncheckedApplyDataToScript redeemer

        Script applied = apply script
       in
        testScript $ ScriptCase testName expectedResult applied applied
   in
    testGroup
      "NFT Minting Policy"
      [ mkTest "Valid mint" validMint ScriptSuccess
      , mkTest "Invalid mint - more than one minted" invalidMintMoreThanOne ScriptFailure
      , mkTest "Invalid mint - invalid name" invalidMintInvalidName ScriptFailure
      , mkTest "Invalid mint - no initial spend" invalidMintNoInitialSpend ScriptFailure
      , mkTest "Invalid mint - initial spend wrong id" invalidMintWrongId ScriptFailure
      , mkTest "Invalid mint - initial spend wrong idx" invalidMintWrongIdx ScriptFailure
      , mkTest "Invalid mint - initial spend idx over 255" invalidMintIdxTooHigh ScriptFailure
      ]
