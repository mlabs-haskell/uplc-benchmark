module UplcBenchmark.ScriptLoader (
  loadScriptFromFile,
  uncheckedApplyDataToScript,
)
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Short (toShort)
import Plutarch (Script (Script))
import PlutusCore qualified as PLC
import PlutusLedgerApi.Common (uncheckedDeserialiseUPLC)
import PlutusLedgerApi.V2 (ToData, toData)
import UntypedPlutusCore (Program (Program), Term (Apply, Constant))

uncheckedApplyDataToScript :: (ToData argument) => argument -> Script -> Script
uncheckedApplyDataToScript argument (Script (Program () version unappliedTerm)) =
  Script
    . Program () version
    . Apply () unappliedTerm
    . Constant ()
    . PLC.someValue
    $ toData argument

loadScriptFromFile :: FilePath -> IO Script
loadScriptFromFile fp = do
  fileContent <- ByteString.readFile fp
  pure $ Script $ uncheckedDeserialiseUPLC $ toShort fileContent
