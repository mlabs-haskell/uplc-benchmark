{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}

module Main (main) where

import Data.ByteString qualified as BS (writeFile)
import Data.ByteString.Short (fromShort)
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V2 (serialiseCompiledCode)
import PlutusTx (compile)
import UplcBenchmark.NftMarketplace (nftMarketplaceValidator)

compiledNftMarketplaceValidator :: SerialisedScript
compiledNftMarketplaceValidator = serialiseCompiledCode $$(compile [||nftMarketplaceValidator||])

compileToFile :: SerialisedScript -> FilePath -> IO ()
compileToFile script fp = BS.writeFile fp $ fromShort script

main :: IO ()
main = do
  compileToFile compiledNftMarketplaceValidator "nft-marketplace-validator.bin"
