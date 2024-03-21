module Main (main) where

import UplcBenchmark (getBinPath, implementations)
import UplcBenchmark.ScriptSize (mkSizeReport, sizeReportsToGnuPlotDat)

main :: IO ()
main = do
  binPaths <- traverse getBinPath implementations
  reports <- traverse mkSizeReport binPaths
  writeFile "script_size.csv" $ sizeReportsToGnuPlotDat reports
