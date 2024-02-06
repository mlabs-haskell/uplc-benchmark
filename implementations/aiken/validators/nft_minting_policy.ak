use aiken/builtin.{choose_data, head_list, null_list, tail_list, un_constr_data}
use aiken/bytearray.{concat, push}
use aiken/cbor.{diagnostic}
use aiken/dict.{to_list}
use aiken/list.{any, head, tail}
use aiken/transaction.{Mint, OutputReference, ScriptContext}
use aiken/transaction/value.{AssetName, from_minted_value, tokens}

type NftMintingPolicyRedeemer {
  initial_spend: OutputReference,
}

fn decode_NftMintingPolicyRedeemer(data: Data) -> NftMintingPolicyRedeemer {
  trace diagnostic(data)

  choose_data(
    data,
    fn(data) {
      let constr = un_constr_data(data)
      let constr_idx = constr.1st
      let lst = constr.2nd
      expect initial_spend: OutputReference = head_list(lst)
      let lst = tail_list(lst)
      if null_list(lst) && constr_idx == 0 {
        NftMintingPolicyRedeemer { initial_spend }
      } else {
        fail
      }
    },
    fn(_) { fail },
    fn(_) { fail },
    fn(_) { fail },
    fn(_) { fail },
  )(
    data,
  )
}

fn int_to_bs(n: Int, acc: ByteArray) -> ByteArray {
  if n < 256 {
    push(acc, n)
  } else {
    int_to_bs(n / 256, push(acc, n % 256))
  }
}

fn derive_nft_name(ref: OutputReference) -> AssetName {
  concat(ref.transaction_id.hash, int_to_bs(ref.output_index, #""))
}

validator {
  fn mint(redeemer_raw: Data, context: ScriptContext) -> Bool {
    let redeemer = decode_NftMintingPolicyRedeemer(redeemer_raw)
    expect Mint(own_symbol) = context.purpose
    let own_mint =
      to_list(tokens(from_minted_value(context.transaction.mint), own_symbol))

    expect Some(minted_nft) = head(own_mint)

    let minted_nft_name = minted_nft.1st
    let minted_nft_amount = minted_nft.2nd

    let minted_one_nft = minted_nft_amount == 1
    let minted_nft_matches_name =
      minted_nft_name == derive_nft_name(redeemer.initial_spend)
    let minted_only_one_asset = tail(own_mint) == Some([])
    let spent_initial_spend =
      any(
        context.transaction.inputs,
        fn(input) { input.output_reference == redeemer.initial_spend },
      )

    minted_one_nft? && minted_nft_matches_name? && minted_only_one_asset? && spent_initial_spend?
  }
}
