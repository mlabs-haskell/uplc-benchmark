module Main (main) where

import UplcBenchmark (getBinPath, implementations)
import UplcBenchmark.ExecutionUnits (writeProfileFile)
import UplcBenchmark.ScriptSize (mkSizeReport, sizeReportsToGnuPlotDat)
import UplcBenchmark.Spec.LpPolicy (mkValidMintOneNft, mkValidMintTwoNfts)
import UplcBenchmark.Spec.NftMarketplace (mkCancelOneTest, mkValidBuyOneTest)
import UplcBenchmark.Spec.PoolNftPolicy (mkValidMint)
import UplcBenchmark.Spec.PoolValidator (mkDeposit, mkSwapAForB, mkSwapBForA, mkWithdraw)

main :: IO ()
main = do
  binPaths <- traverse getBinPath implementations
  reports <- traverse mkSizeReport binPaths
  writeFile "script_size.csv" $ sizeReportsToGnuPlotDat reports

  writeProfileFile "budget_buy_one.csv" "nft-marketplace-validator.bin" mkValidBuyOneTest
  writeProfileFile "budget_cancel_one.csv" "nft-marketplace-validator.bin" mkCancelOneTest

  writeProfileFile "budget_lp_mint_one_nft.csv" "lp-minting-policy.bin" mkValidMintOneNft
  writeProfileFile "budget_lp_mint_two_nfts.csv" "lp-minting-policy.bin" mkValidMintTwoNfts

  writeProfileFile "budget_nft_mint.csv" "nft-minting-policy.bin" mkValidMint

  writeProfileFile "budget_swap_a_for_b.csv" "pool-validator.bin" mkSwapAForB
  writeProfileFile "budget_swap_b_for_a.csv" "pool-validator.bin" mkSwapBForA
  writeProfileFile "budget_deposit.csv" "pool-validator.bin" mkDeposit
  writeProfileFile "budget_withdraw.csv" "pool-validator.bin" mkWithdraw
