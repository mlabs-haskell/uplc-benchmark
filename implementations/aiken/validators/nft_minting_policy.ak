use aiken/bytearray
use aiken/dict
use aiken/list
use aiken/transaction.{Input, Mint, ScriptContext, OutputReference}
use aiken/transaction/value.{AssetName}

fn int_to_bs(n: Int, acc: ByteArray) -> ByteArray {
  if n < 256 {
    bytearray.push(acc, n)
  } else {
    int_to_bs(n / 256, bytearray.push(acc, n % 256))
  }
}

fn derive_nft_name(ref: OutputReference) -> AssetName {
  bytearray.concat(
    ref.transaction_id.hash,
    int_to_bs(ref.output_index, ""),
  )
}

validator {
  fn mint(redeemer: OutputReference, ctx: ScriptContext) {
    expect ScriptContext { purpose: Mint(own_symbol), transaction } = ctx

    expect [Pair(minted_nft_name, 1)] =
      dict.to_pairs(value.tokens(value.from_minted_value(transaction.mint), own_symbol))

    expect minted_nft_name == derive_nft_name(redeemer)

    list.any(transaction.inputs, fn(input) { input.output_reference == redeemer })
  }
}
