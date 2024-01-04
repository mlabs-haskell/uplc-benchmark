module Main (main) where

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

testsFromDir :: (String, FilePath) -> IO TestTree
testsFromDir (testName, baseFilePath) = do
  nftMarketplaceScript <- loadScriptFromFile (baseFilePath </> "nft-marketplace-validator.bin")
  pure $ testGroup testName [NftMarketplace.specForScript nftMarketplaceScript]

getEnv :: String -> IO String
getEnv env = do
  value <- lookupEnv env
  case value of
    Just v -> pure v
    Nothing -> do
      ePutStrLn ("ERROR: " <> env <> " is not set")
      exitFailure

main :: IO ()
main = do
  plutarchPath <- getEnv "UPLC_BENCHMARK_BIN_PLUTARCH"
  tests <- traverse testsFromDir [("Plutarch", plutarchPath)]
  defaultMain $ testGroup "UPLC Benchmark" tests
