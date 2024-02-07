use aiken/builtin.{
  choose_data, head_list, null_list, tail_list, un_constr_data, un_i_data,
  un_list_data,
}
use aiken/list.{find}
use aiken/transaction.{InlineDatum, ScriptContext, Spend}
use aiken/transaction/value.{
  AssetName, PolicyId, Value, from_minted_value, quantity_of,
}

type AssetClass {
  symbol: PolicyId,
  name: AssetName,
}

fn decode_AssetClass(data: Data) -> AssetClass {
  choose_data(
    data,
    fn(data) {
      let constr = un_constr_data(data)
      let constr_idx = constr.1st
      let lst = constr.2nd

      expect symbol: PolicyId = head_list(lst)
      let lst = tail_list(lst)

      expect name: AssetName = head_list(lst)
      let lst = tail_list(lst)

      if null_list(lst) && constr_idx == 0 {
        AssetClass { symbol, name }
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

type DexDatum {
  token_a: AssetClass,
  token_b: AssetClass,
  pool_nft: AssetClass,
  lp_token: PolicyId,
  minted_lp_tokens: Int,
  swap_fee: Int,
}

fn decode_DexDatum(data: Data) -> DexDatum {
  choose_data(
    data,
    fn(_) { fail },
    fn(_) { fail },
    fn(data) {
      let lst = un_list_data(data)

      let token_a: AssetClass = decode_AssetClass(head_list(lst))
      let lst = tail_list(lst)

      let token_b: AssetClass = decode_AssetClass(head_list(lst))
      let lst = tail_list(lst)

      let pool_nft: AssetClass = decode_AssetClass(head_list(lst))
      let lst = tail_list(lst)

      expect lp_token: PolicyId = head_list(lst)
      let lst = tail_list(lst)

      expect minted_lp_tokens: Int = head_list(lst)
      let lst = tail_list(lst)

      expect swap_fee: Int = head_list(lst)
      let lst = tail_list(lst)

      if null_list(lst) {
        DexDatum {
          token_a,
          token_b,
          pool_nft,
          lp_token,
          minted_lp_tokens,
          swap_fee,
        }
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

type DexRedeemer {
  Swap
  DepositLiquidity
  WithdrawLiquidity
}

fn decode_DexRedeemer(data: Data) -> DexRedeemer {
  choose_data(
    data,
    fn(_) { fail },
    fn(_) { fail },
    fn(_) { fail },
    fn(data) {
      let i = un_i_data(data)
      if i == 0 {
        Swap
      } else if i == 1 {
        DepositLiquidity
      } else if i == 2 {
        WithdrawLiquidity
      } else {
        fail
      }
    },
    fn(_) { fail },
  )(
    data,
  )
}

fn quantity_of_asset(value: Value, asset_class: AssetClass) -> Int {
  quantity_of(value, asset_class.symbol, asset_class.name)
}

fn get_diff(new: Int, old: Int) -> Int {
  if new - old < 0 {
    0
  } else {
    new - old
  }
}

// The uniswap fee is 0.3%; here it is multiplied by 1000, so that the
// on-chain code deals only in integers.
// See: <https://uniswap.org/whitepaper.pdf> Eq (11) (Page 7.)
const fee_den: Int = 1000

fn check_swap(
  fee_num: Int,
  old_a: Int,
  old_b: Int,
  new_a: Int,
  new_b: Int,
) -> Bool {
  fee_den * fee_den * old_a * old_b <= (
    new_a * fee_den - get_diff(new_a, old_a) * fee_num
  ) * ( new_b * fee_den - get_diff(new_b, old_b) * fee_num )
}

validator {
  fn spend(datum_raw: Data, redeemer_raw: Data, context: ScriptContext) -> Bool {
    let input_datum = decode_DexDatum(datum_raw)
    let redeemer = decode_DexRedeemer(redeemer_raw)

    expect Spend(own_input_ref) = context.purpose
    expect Some(own_input) =
      find(
        context.transaction.inputs,
        fn(input) { input.output_reference == own_input_ref },
      )

    expect Some(own_output) =
      find(
        context.transaction.outputs,
        fn(output) {
          quantity_of_asset(output.value, input_datum.pool_nft) == 1
        },
      )

    let new_minted_lp =
      quantity_of(
        from_minted_value(context.transaction.mint),
        input_datum.lp_token,
        // lp name == nft name
        input_datum.pool_nft.name,
      )

    let in_a_amount =
      quantity_of_asset(own_input.output.value, input_datum.token_a)
    let in_b_amount =
      quantity_of_asset(own_input.output.value, input_datum.token_b)

    let out_a_amount = quantity_of_asset(own_output.value, input_datum.token_a)
    let out_b_amount = quantity_of_asset(own_output.value, input_datum.token_b)

    expect InlineDatum(output_datum_raw) = own_output.datum
    let output_datum = decode_DexDatum(output_datum_raw)

    let input_has_nft =
      quantity_of_asset(own_input.output.value, input_datum.pool_nft) == 1
    let output_no_ref_script = own_output.reference_script == None
    let valid_token_a = input_datum.token_a == output_datum.token_a
    let valid_token_b = input_datum.token_b == output_datum.token_b
    let valid_pool_nft = input_datum.pool_nft == output_datum.pool_nft
    let valid_lp_token = input_datum.lp_token == output_datum.lp_token
    let valid_swap_fee = input_datum.swap_fee == output_datum.swap_fee

    let common_checks =
      input_has_nft? && output_no_ref_script? && valid_token_a? && valid_token_b? && valid_pool_nft? && valid_lp_token? && valid_swap_fee?

    when redeemer is {
      DepositLiquidity -> {
        let valid_mint_lp =
          input_datum.minted_lp_tokens + new_minted_lp == output_datum.minted_lp_tokens
        let no_burn_lp = new_minted_lp > 0
        let deposit_enough =
          output_datum.minted_lp_tokens * output_datum.minted_lp_tokens <= out_a_amount * out_b_amount

        valid_mint_lp? && no_burn_lp? && deposit_enough? && common_checks?
      }
      WithdrawLiquidity -> {
        let valid_mint_lp =
          input_datum.minted_lp_tokens + new_minted_lp == output_datum.minted_lp_tokens
        let no_mint_lp = new_minted_lp < 0
        let withdrawn_not_too_much =
          output_datum.minted_lp_tokens * output_datum.minted_lp_tokens <= out_a_amount * out_b_amount

        valid_mint_lp? && no_mint_lp? && withdrawn_not_too_much? && common_checks?
      }
      Swap -> {
        let valid_lp =
          input_datum.minted_lp_tokens == output_datum.minted_lp_tokens
        let no_mint_lp = new_minted_lp == 0
        let swapped_not_too_much =
          check_swap(
            input_datum.swap_fee,
            in_a_amount,
            in_b_amount,
            out_a_amount,
            out_b_amount,
          )
        valid_lp? && no_mint_lp? && swapped_not_too_much && common_checks?
      }
    }
  }
}
