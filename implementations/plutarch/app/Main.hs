module Main (main) where

import Data.ByteString qualified as BS (writeFile)
import Data.ByteString.Short (fromShort)
import Data.Text qualified as Text
import Plutarch (ClosedTerm, Config (Config), TracingMode (NoTracing), compile)
import Plutarch.Script (serialiseScript)
import System.IO (hPutStrLn, stderr)

import UplcBenchmark.NftMarketplace (pnftMarketplaceValidator)

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

compilationConfig :: Config
compilationConfig = Config NoTracing

compileToFile :: ClosedTerm a -> FilePath -> IO ()
compileToFile term fp = do
  case compile compilationConfig term of
    Right script -> do
      let serialised = fromShort $ serialiseScript script
      BS.writeFile fp serialised
    Left err -> do
      ePutStrLn $ "error: could not compile '" <> fp <> "'."
      ePutStrLn $ Text.unpack err

main :: IO ()
main = do
  compileToFile pnftMarketplaceValidator "nft-marketplace-validator.bin"
