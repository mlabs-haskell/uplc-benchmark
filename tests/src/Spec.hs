module Main (main) where

import Plutarch.Context ()
import System.FilePath ((</>))
import Test.Tasty (defaultMain)

import UplcBenchmark.ScriptLoader (loadScriptFromFile)
import UplcBenchmark.Spec.NftMarketplace (specForScript)

main :: IO ()
main = do
  let
    baseFilePath :: FilePath
    baseFilePath = "/nix/store/6hqnhwfn72lxbqy9s9kxr6k2yw8dmv98-plutarch-implementation-compiled"
  script <- loadScriptFromFile (baseFilePath </> "nft-marketplace-validator.bin")
  defaultMain $ specForScript script
  pure ()
