use aiken/dict
use aiken/list
use aiken/transaction.{Input, Mint, ScriptContext}
use aiken/transaction/value.{AssetName, PolicyId}

validator(pool_nft_cs: PolicyId) {
  fn mint(_: Data, ctx: ScriptContext) -> Bool {
    expect ScriptContext { purpose: Mint(own_symbol), transaction } = ctx

    let has_nft =
      fn(pool_nft_tn: AssetName, tx_in_info: Input) -> Bool {
        value.quantity_of(tx_in_info.output.value, pool_nft_cs, pool_nft_tn) == 1
      }

    transaction.mint
      |> value.from_minted_value
      |> value.tokens(own_symbol)
      |> dict.foldr(True, fn(minted_lp_name, _minted_lp_quantity, accum) { and {
              accum,
              list.any(transaction.inputs, has_nft(minted_lp_name, _)),
            } })
  }
}
