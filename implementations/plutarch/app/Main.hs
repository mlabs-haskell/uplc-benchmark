module Main (main) where

import Data.ByteString qualified as BS (writeFile)
import Data.ByteString.Short (fromShort)
import Data.Kind (Type)
import Data.Text qualified as Text
import Plutarch.Internal.Term (Config (NoTracing), compile)
import Plutarch.Prelude (S, Term)
import Plutarch.Script (serialiseScript)
import System.IO (hPutStrLn, stderr)

import UplcBenchmark.LpMintingPolicy (plpMintingPolicy)
import UplcBenchmark.NftMarketplace (pnftMarketplaceValidator)
import UplcBenchmark.NftMintingPolicy (pnftMintingPolicy)
import UplcBenchmark.PoolValidator (ppoolValidator)

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

compilationConfig :: Config
compilationConfig = NoTracing

compileToFile :: forall (a :: S -> Type). (forall (s :: S). Term s a) -> FilePath -> IO ()
compileToFile term fp = do
  ePutStrLn $ "info: compiling '" <> fp <> "'"
  case compile compilationConfig term of
    Right script -> do
      let serialised = fromShort $ serialiseScript script
      BS.writeFile fp serialised
      ePutStrLn $ "info: serialized '" <> fp <> "'"
    Left err -> do
      ePutStrLn $ "error: could not compile '" <> fp <> "'"
      ePutStrLn $ Text.unpack err

main :: IO ()
main = do
  compileToFile pnftMarketplaceValidator "nft-marketplace-validator.bin"
  compileToFile plpMintingPolicy "lp-minting-policy.bin"
  compileToFile pnftMintingPolicy "nft-minting-policy.bin"
  compileToFile ppoolValidator "pool-validator.bin"
