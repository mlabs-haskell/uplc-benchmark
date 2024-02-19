{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}

module Main (main) where

import Data.ByteString qualified as BS (writeFile)
import Data.ByteString.Short (fromShort)
import PlutusLedgerApi.V2 (serialiseCompiledCode)
import PlutusTx (CompiledCode, compile)
import UplcBenchmark.LpMintingPolicy (lpMintingPolicy)
import UplcBenchmark.NftMarketplace (nftMarketplaceValidator)
import UplcBenchmark.NftMintingPolicy (nftMintingPolicy)
import UplcBenchmark.PoolValidator (poolValidator)

compileToFile :: CompiledCode script -> FilePath -> IO ()
compileToFile script fp =
  BS.writeFile fp $ fromShort $ serialiseCompiledCode script

main :: IO ()
main = do
  compileToFile $$(compile [||nftMarketplaceValidator||]) "nft-marketplace-validator.bin"
  compileToFile $$(compile [||lpMintingPolicy||]) "lp-minting-policy.bin"
  compileToFile $$(compile [||nftMintingPolicy||]) "nft-minting-policy.bin"
  compileToFile $$(compile [||poolValidator||]) "pool-validator.bin"
