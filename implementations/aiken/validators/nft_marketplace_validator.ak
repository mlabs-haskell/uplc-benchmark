use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use cardano/address.{Address}
use cardano/assets
use cardano/transaction.{InlineDatum, Transaction}

type Datum =
  (Data, Address, VerificationKeyHash)

validator nft_marketplace_validator {
  spend(datum: Option<Datum>, redeemer: Int, own_input: Data, self: Transaction) {
    expect Some((price, seller, cancel_key)) = datum
    when redeemer is {
      // Buy
      0 -> {
        let has_valid_payment =
          list.any(
            self.outputs,
            fn(output) {
              when output.datum is {
                InlineDatum(inline_datum) -> and {
                    output.address == seller,
                    assets.match(output.value, price, ==),
                    inline_datum == own_input,
                  }
                _ -> False
              }
            },
          )
        has_valid_payment
      }
      // Cancel
      1 -> {
        let signed_by_owner = list.has(self.extra_signatories, cancel_key)
        signed_by_owner
      }
      _ -> fail
    }
  }

  else(_) {
    fail
  }
}
