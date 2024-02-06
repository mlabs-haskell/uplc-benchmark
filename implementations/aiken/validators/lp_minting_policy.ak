use aiken/dict.{to_list}
use aiken/list.{all, any}
use aiken/transaction.{Input, Mint, ScriptContext}
use aiken/transaction/value.{
  AssetName, PolicyId, from_minted_value, quantity_of, tokens,
}

// aiken doesn't allow nested functions
fn has_nft(
  pool_nft_cs: PolicyId,
  pool_nft_tn: AssetName,
  tx_in_info: Input,
) -> Bool {
  quantity_of(tx_in_info.output.value, pool_nft_cs, pool_nft_tn) == 1
}

validator(pool_nft_cs: PolicyId) {
  fn mint(_: Data, context: ScriptContext) -> Bool {
    expect Mint(own_symbol) = context.purpose

    let own_mint =
      to_list(tokens(from_minted_value(context.transaction.mint), own_symbol))
    let is_valid =
      all(
        own_mint,
        fn(minted_pair) {
          let minted_lp_name = minted_pair.1st
          any(
            context.transaction.inputs,
            fn(input) { has_nft(pool_nft_cs, minted_lp_name, input) },
          )
        },
      )

    is_valid
  }
}
