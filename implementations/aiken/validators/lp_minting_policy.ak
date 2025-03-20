use aiken/collection/dict
use aiken/collection/list
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{Input, Transaction}

validator lp_minting_policy(pool_nft_cs: PolicyId) {
  mint(_: Data, own_symbol: PolicyId, self: Transaction) {
    let has_nft =
      fn(pool_nft_tn: AssetName, tx_in_info: Input) -> Bool {
        assets.quantity_of(tx_in_info.output.value, pool_nft_cs, pool_nft_tn) == 1
      }

    self.mint
      |> assets.tokens(own_symbol)
      |> dict.foldr(
          True,
          fn(minted_lp_name, _minted_lp_quantity, accum) {
            and {
              accum,
              list.any(self.inputs, has_nft(minted_lp_name, _)),
            }
          },
        )
  }

  else(_) {
    fail
  }
}
