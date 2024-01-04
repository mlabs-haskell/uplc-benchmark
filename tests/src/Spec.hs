module Main (main) where

import Plutarch.Context ()
import System.FilePath ((</>))
import Test.Tasty (defaultMain, testGroup)

import UplcBenchmark.ScriptLoader (loadScriptFromFile)
import UplcBenchmark.Spec.NftMarketplace (specForScript)

main :: IO ()
main = do
  let
    -- TODO: Get this from env
    baseFilePath :: FilePath
    baseFilePath = "../implementations/plutarch"

  script <- loadScriptFromFile (baseFilePath </> "nft-marketplace-validator.bin")
  defaultMain $
    testGroup
      "UPLC Benchmark"
      [ testGroup "Plutarch" [specForScript script]
      ]

  pure ()
