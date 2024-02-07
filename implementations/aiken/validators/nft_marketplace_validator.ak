use aiken/builtin.{
  choose_data, head_list, null_list, tail_list, un_i_data, un_list_data,
}
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{InlineDatum, ScriptContext, Spend}
use aiken/transaction/credential.{Address, VerificationKey}
use aiken/transaction/value.{Value}

// Aiken doesn't support custom data encodings so we must roll them out by hand
// Protocol must follow data encodings from specification, not some arbitrary
// format chosen by aiken developers.

type NftMarketplaceDatum {
  price: Value,
  seller: Address,
  cancel_key: Hash<Blake2b_224, VerificationKey>,
}

fn decode_NftMarketplaceDatum(data: Data) -> NftMarketplaceDatum {
  choose_data(
    data,
    fn(_) { fail },
    fn(_) { fail },
    fn(_) {
      let lst = un_list_data(data)
      expect price: Value = head_list(lst)
      let lst = tail_list(lst)
      expect seller: Address = head_list(lst)
      let lst = tail_list(lst)
      expect cancel_key: Hash<Blake2b_224, VerificationKey> = head_list(lst)
      let lst = tail_list(lst)
      if null_list(lst) {
        NftMarketplaceDatum { price, seller, cancel_key }
      } else {
        fail
      }
    },
    fn(_) { fail },
    fn(_) { fail },
  )(
    data,
  )
}

type NftMarketplaceRedeemer {
  Buy
  Cancel
}

fn decode_NftMarketplaceRedeemer(data: Data) -> NftMarketplaceRedeemer {
  choose_data(
    data,
    fn(_) { fail },
    fn(_) { fail },
    fn(_) { fail },
    fn(data) {
      let i = un_i_data(data)
      if i == 0 {
        Buy
      } else if i == 1 {
        Cancel
      } else {
        fail
      }
    },
    fn(_) { fail },
  )(
    data,
  )
}

validator {
  fn spend(datum_raw: Data, redeemer_raw: Data, context: ScriptContext) -> Bool {
    let datum = decode_NftMarketplaceDatum(datum_raw)
    let redeemer = decode_NftMarketplaceRedeemer(redeemer_raw)

    expect Spend(own_input) = context.purpose
    let own_input_data: Data = own_input
    when redeemer is {
      Buy -> {
        let has_valid_payment =
          list.any(
            context.transaction.outputs,
            fn(output) {
              when output.datum is {
                InlineDatum(inline_datum) ->
                  output.address == datum.seller && output.value == datum.price && inline_datum == own_input_data
                _ -> False
              }
            },
          )
        has_valid_payment
      }
      Cancel -> {
        let signed_by_owner =
          list.has(context.transaction.extra_signatories, datum.cancel_key)
        signed_by_owner
      }
    }
  }
}
