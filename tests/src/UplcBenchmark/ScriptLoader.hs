module UplcBenchmark.ScriptLoader (
  loadScriptFromFile,
  uncheckedApplyDataToScript,
) where

import Data.ByteString qualified as ByteString
import Data.ByteString.Short (toShort)
import Plutarch (Script (Script))
import PlutusCore qualified as PLC
import PlutusLedgerApi.Common (uncheckedDeserialiseUPLC)
import PlutusLedgerApi.V2 (ToData, toData)
import UntypedPlutusCore (
  DeBruijn,
  DefaultFun,
  DefaultUni,
  Program (Program),
  Term (Apply, Constant),
 )

getScript :: Script -> Program DeBruijn DefaultUni DefaultFun ()
getScript (Script program) = program

uncheckedApplyDataToScript :: (ToData argument) => argument -> Script -> Script
uncheckedApplyDataToScript argument unappliedScript =
  let Program () version unappliedTerm = getScript unappliedScript
   in Script
        . Program () version
        . Apply () unappliedTerm
        . Constant ()
        . PLC.someValue
        $ toData argument

loadScriptFromFile :: FilePath -> IO Script
loadScriptFromFile fp = do
  fileContent <- ByteString.readFile fp
  pure $ Script $ uncheckedDeserialiseUPLC $ toShort fileContent
