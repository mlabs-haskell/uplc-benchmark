use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{InlineDatum, ScriptContext, Spend}
use aiken/transaction/credential.{Address, VerificationKey}

type Datum =
  (Data, Address, Hash<Blake2b_224, VerificationKey>)

fn as_data(data: Data) -> Data {
  data
}

validator {
  fn spend(datum: Datum, redeemer: Int, ctx: ScriptContext) {
    expect ScriptContext { purpose: Spend(own_input), transaction } = ctx
    let (price, seller, cancel_key) = datum
    when redeemer is {
      // Buy
      0 -> {
        let has_valid_payment =
          list.any(
            transaction.outputs,
            fn(output) {
              when output.datum is {
                InlineDatum(inline_datum) -> and {
                    output.address == seller,
                    as_data(output.value) == price,
                    inline_datum == as_data(own_input),
                  }
                _ -> False
              }
            },
          )
        has_valid_payment
      }
      // Cancel
      1 -> {
        let signed_by_owner =
          list.has(transaction.extra_signatories, cancel_key)
        signed_by_owner
      }
      _ -> fail
    }
  }
}
