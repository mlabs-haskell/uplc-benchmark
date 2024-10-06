module Main (main) where

import UplcBenchmark (getBinPath, implementations)
import UplcBenchmark.ExecutionUnits (writeProfileFile)
import UplcBenchmark.ScriptSize (mkSizeReport, sizeReportsToGnuPlotDat)
import UplcBenchmark.Spec.NftMarketplace (mkCancelOneTest, mkValidBuyOneTest)

main :: IO ()
main = do
  binPaths <- traverse getBinPath implementations
  reports <- traverse mkSizeReport binPaths
  writeFile "script_size.csv" $ sizeReportsToGnuPlotDat reports

  writeProfileFile "budget_buy_one.csv" "nft-marketplace-validator.bin" mkValidBuyOneTest
  writeProfileFile "budget_cancel_one.csv" "nft-marketplace-validator.bin" mkCancelOneTest
