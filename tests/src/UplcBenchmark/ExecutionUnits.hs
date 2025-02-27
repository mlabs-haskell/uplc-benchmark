module UplcBenchmark.ExecutionUnits (writeProfileFile, profileScripts, profiledToCsv, Profiled) where

import Data.Kind (Type)
import GHC.Stack (HasCallStack)
import Plutarch.Script (Script)
import Plutarch.Test.Program (ScriptCase (ScriptCase))
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (ExBudget))
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParametersForTesting)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (ExCPU), ExMemory (ExMemory))
import System.FilePath ((</>))
import UntypedPlutusCore (DefaultFun, DefaultUni, termMapNames)
import UntypedPlutusCore.Core (Program (Program), Term)
import UntypedPlutusCore.DeBruijn (NamedDeBruijn, fakeNameDeBruijn)
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek

import UplcBenchmark (Implementation (Implementation), getEnv, implementations)
import UplcBenchmark.ScriptLoader (loadScriptFromFile)

writeProfileFile :: FilePath -> FilePath -> (Script -> ScriptCase) -> IO ()
writeProfileFile outFile scriptFile mkTest = do
  profiled <- profileScripts scriptFile mkTest
  writeFile outFile $ profiledToCsv profiled

profileScripts :: (HasCallStack) => FilePath -> (Script -> ScriptCase) -> IO [Profiled]
profileScripts scriptPath mkTest = traverse (profileScript scriptPath mkTest) implementations

profileScript :: (HasCallStack) => FilePath -> (Script -> ScriptCase) -> Implementation -> IO Profiled
profileScript scriptPath mkTest (Implementation implementationName baseFilePathEnv) = do
  baseFilePath <- getEnv baseFilePathEnv
  script <- loadScriptFromFile (baseFilePath </> scriptPath)
  let (ScriptCase _ _ (Program _ _ term) _) = mkTest script
  let usedBudget = evalTerm $ termMapNames fakeNameDeBruijn term
  pure
    Profiled
      { profiled'implementation = implementationName
      , profiled'budget = usedBudget
      }

type Profiled :: Type
data Profiled = Profiled
  { profiled'implementation :: String
  , profiled'budget :: ExBudget
  }

profiledToCsv :: [Profiled] -> String
profiledToCsv profiled =
  unlines
    ( [ "Language,CPU,Memory"
      ]
        <> map
          ( \(Profiled language (ExBudget (ExCPU cpu) (ExMemory mem))) ->
              mconcat [language, ",", show cpu, ",", show mem]
          )
          profiled
    )

evalTerm :: (HasCallStack) => Term NamedDeBruijn DefaultUni DefaultFun () -> ExBudget
evalTerm term =
  case Cek.runCekDeBruijn defaultCekParametersForTesting Cek.counting Cek.logEmitter term of
    (Right _, Cek.CountingSt budget, _logs) -> budget
    (Left err, _, t) -> error $ "evalTerm: Script failure: " <> show err <> ". Log: " <> show t
