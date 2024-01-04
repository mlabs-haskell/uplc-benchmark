module UplcBenchmark.Spec.NftMarketplace (specForScript) where

import Data.ByteString qualified as ByteString
import Data.ByteString.Hash (blake2b_256)
import Data.Word (Word8)
import Plutarch (Script)
import Plutarch.Context (SpendingBuilder, buildSpending', withSpendingOutRef)
import Plutarch.Test.Script (
  ScriptCase (ScriptCase),
  ScriptResult (ScriptSuccess),
  testScript,
 )
import PlutusLedgerApi.V2 (ScriptContext, TxId (TxId), TxOutRef (TxOutRef), toBuiltin)
import Test.Tasty (TestTree, testGroup)
import UplcBenchmark.ScriptLoader (uncheckedApplyDataToScript)

mkTxId :: Word8 -> TxId
mkTxId input = TxId $ toBuiltin $ blake2b_256 $ ByteString.pack [input]

validatedOrderUTxO :: SpendingBuilder
validatedOrderUTxO =
  withSpendingOutRef $
    TxOutRef (mkTxId 0) 0

validSpendOne :: ScriptContext
validSpendOne =
  buildSpending' $
    mconcat
      [ validatedOrderUTxO
      ]

specForScript :: Script -> TestTree
specForScript script =
  let
    mkTest :: String -> ScriptContext -> ScriptResult -> TestTree
    mkTest testName context expectedResult =
      let applied =
            uncheckedApplyDataToScript context
              . uncheckedApplyDataToScript context
              . uncheckedApplyDataToScript context
              $ script
       in testScript $ ScriptCase testName expectedResult applied applied
   in
    testGroup "" [mkTest "Valid one spend" validSpendOne ScriptSuccess]
