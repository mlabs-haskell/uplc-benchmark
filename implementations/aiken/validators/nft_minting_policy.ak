use aiken/collection/dict
use aiken/collection/list
use aiken/primitive/bytearray
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{OutputReference, Transaction}

fn derive_nft_name(ref: OutputReference) -> AssetName {
  bytearray.concat(
    ref.transaction_id,
    bytearray.from_int_big_endian(ref.output_index, 0),
  )
}

validator nft_minting_policy {
  mint(redeemer: OutputReference, own_symbol: PolicyId, self: Transaction) {
    expect [Pair(minted_nft_name, 1)] =
      dict.to_pairs(assets.tokens(self.mint, own_symbol))

    expect minted_nft_name == derive_nft_name(redeemer)

    list.any(self.inputs, fn(input) { input.output_reference == redeemer })
  }

  else(_) {
    fail
  }
}
