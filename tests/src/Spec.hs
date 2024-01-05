module Main (main) where

import Data.Kind (Type)
import Plutarch.Context ()
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Test.Tasty (TestTree, defaultMain, testGroup)
import UplcBenchmark.ScriptLoader (loadScriptFromFile)
import UplcBenchmark.Spec.NftMarketplace qualified as NftMarketplace (specForScript)

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

mkTestForImplementation :: Implementation -> IO TestTree
mkTestForImplementation (Implementation testName baseFilePathEnv) = do
  baseFilePath <- getEnv baseFilePathEnv
  nftMarketplaceScript <- loadScriptFromFile (baseFilePath </> "nft-marketplace-validator.bin")
  pure $
    testGroup
      testName
      [ NftMarketplace.specForScript nftMarketplaceScript
      ]

implementations :: [Implementation]
implementations =
  [ Implementation "Plutarch" "UPLC_BENCHMARK_BIN_PLUTARCH"
  ]

main :: IO ()
main = do
  allTests <- traverse mkTestForImplementation implementations
  defaultMain $ testGroup "UPLC Benchmark" allTests
