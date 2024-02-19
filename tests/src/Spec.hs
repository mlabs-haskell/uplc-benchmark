module Main (main) where

import Data.Kind (Type)
import Data.Tagged (Tagged (Tagged))
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Providers (IsTest (run, testOptions))
import Test.Tasty.Providers.ConsoleFormat (ResultDetailsPrinter (ResultDetailsPrinter))
import Test.Tasty.Runners (FailureReason (TestFailed), Outcome (Failure), Result (Result), TestTree (SingleTest))
import UplcBenchmark.ScriptLoader (loadScriptFromFile)
import UplcBenchmark.Spec.LpPolicy qualified as LpPolicy (specForScript)
import UplcBenchmark.Spec.NftMarketplace qualified as NftMarketplace (specForScript)
import UplcBenchmark.Spec.PoolNftPolicy qualified as PoolNftPolicy (specForScript)
import UplcBenchmark.Spec.PoolValidator qualified as PoolValidator (specForScript)

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

getEnv :: String -> IO String
getEnv env = do
  value <- lookupEnv env
  case value of
    Just v -> pure v
    Nothing -> do
      ePutStrLn ("ERROR: '" <> env <> "' environment variable is not set. Set it to a path with compiled validators.")
      exitFailure

type Implementation :: Type
data Implementation = Implementation String FilePath

type MissingValidatorFile :: Type
data MissingValidatorFile = MissingValidatorFile

instance IsTest MissingValidatorFile where
  run _ _ _ = pure $ Result (Failure TestFailed) "File does not exist" "" 0.0 (ResultDetailsPrinter $ \_ _ -> pure ())
  testOptions = Tagged []

mkTestForImplementation :: Implementation -> IO TestTree
mkTestForImplementation (Implementation testName baseFilePathEnv) = do
  baseFilePath <- getEnv baseFilePathEnv
  let mkTestTreeForScript mkTest scriptPath = do
        scriptExists <- doesFileExist (baseFilePath </> scriptPath)
        if scriptExists
          then do
            script <- loadScriptFromFile (baseFilePath </> scriptPath)
            pure $ mkTest script
          else pure $ SingleTest scriptPath MissingValidatorFile

  nftMarketplaceTests <- mkTestTreeForScript NftMarketplace.specForScript "nft-marketplace-validator.bin"
  lpMintingPolicyTests <- mkTestTreeForScript LpPolicy.specForScript "lp-minting-policy.bin"
  nftMintingPolicyTests <- mkTestTreeForScript PoolNftPolicy.specForScript "nft-minting-policy.bin"
  poolValidatorTests <- mkTestTreeForScript PoolValidator.specForScript "pool-validator.bin"

  pure $
    testGroup
      testName
      [ nftMarketplaceTests
      , testGroup
          "DEX"
          [ lpMintingPolicyTests
          , nftMintingPolicyTests
          , poolValidatorTests
          ]
      ]

implementations :: [Implementation]
implementations =
  [ Implementation "Plutarch" "UPLC_BENCHMARK_BIN_PLUTARCH"
  , Implementation "Aiken" "UPLC_BENCHMARK_BIN_AIKEN"
  , Implementation "PlutusTx" "UPLC_BENCHMARK_BIN_PLUTUS_TX"
  ]

main :: IO ()
main = do
  allTests <- traverse mkTestForImplementation implementations
  defaultMain $ testGroup "UPLC Benchmark" allTests
