module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import UplcBenchmark (implementations, mkTestForImplementation)

main :: IO ()
main = do
  allTests <- traverse mkTestForImplementation implementations
  defaultMain $ testGroup "UPLC Benchmark" allTests
