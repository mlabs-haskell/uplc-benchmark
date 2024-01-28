use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{InlineDatum, ScriptContext, Spend}
use aiken/transaction/credential.{Address, VerificationKey}
use aiken/transaction/value.{Value}

type NftMarketplaceDatum {
  price: Value,
  seller: Address,
  cancel_key: Hash<Blake2b_224, VerificationKey>,
}

type NftMarketplaceRedeemer {
  Buy
  Cancel
}

validator {
  fn spend(
    datum: NftMarketplaceDatum,
    redeemer: NftMarketplaceRedeemer,
    context: ScriptContext,
  ) -> Bool {
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
