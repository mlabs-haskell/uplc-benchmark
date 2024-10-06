module Main (main) where

import UplcBenchmark (getBinPath, implementations)
import UplcBenchmark.ExecutionUnits (profileScripts, profiledToCsv)
import UplcBenchmark.ScriptSize (mkSizeReport, sizeReportsToGnuPlotDat)
import UplcBenchmark.Spec.NftMarketplace (mkValidBuyOneTest)

main :: IO ()
main = do
  binPaths <- traverse getBinPath implementations
  reports <- traverse mkSizeReport binPaths
  writeFile "script_size.csv" $ sizeReportsToGnuPlotDat reports
  profiledBuyOne <- profileScripts "nft-marketplace-validator.bin" mkValidBuyOneTest
  writeFile "budget_buy_one.csv" $ profiledToCsv profiledBuyOne
