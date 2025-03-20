use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/math
use cardano/assets.{AssetName, PolicyId}
use cardano/transaction.{InlineDatum, OutputReference, Transaction}

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
    new_a * fee_den - math.max(new_a - old_a, 0) * fee_num
  ) * ( new_b * fee_den - math.max(new_b - old_b, 0) * fee_num )
}

pub type AssetClass {
  symbol: PolicyId,
  name: AssetName,
}

type Datum =
  (AssetClass, AssetClass, AssetClass, PolicyId, Int, Int)

fn collect_amounts(
  value: Pairs<PolicyId, Dict<AssetName, Int>>,
  token_a: AssetClass,
  token_b: AssetClass,
  return: fn(Int, Int) -> a,
) -> a {
  when value is {
    [head, ..tail] ->
      if head.1st == token_a.symbol {
        expect Some(amount_a) = dict.get(head.2nd, token_a.name)
        collect_next(tail, token_b, return(amount_a, _))
      } else if head.1st == token_b.symbol {
        expect Some(amount_b) = dict.get(head.2nd, token_b.name)
        collect_next(tail, token_b, return(_, amount_b))
      } else {
        collect_amounts(tail, token_a, token_b, return)
      }
    [] -> fail @"empty"
  }
}

fn collect_next(
  value: Pairs<PolicyId, Dict<AssetName, Int>>,
  token: AssetClass,
  return: fn(Int) -> a,
) -> a {
  when value is {
    [head, ..tail] ->
      if head.1st == token.symbol {
        expect Some(amount) = dict.get(head.2nd, token.name)
        return(amount)
      } else {
        collect_next(tail, token, return)
      }
    [] -> fail @"empty next"
  }
}

validator pool_validator {
  spend(
    datum: Option<Datum>,
    redeemer: Int,
    own_input_ref: OutputReference,
    self: Transaction,
  ) {
    expect Some((
      token_a,
      token_b,
      pool_nft,
      lp_token,
      minted_lp_tokens,
      swap_fee,
    )) = datum

    expect Some(own_input) =
      list.find(
        self.inputs,
        fn(input) { input.output_reference == own_input_ref },
      )

    expect Some(own_output) =
      list.find(
        self.outputs,
        fn(output) {
          assets.quantity_of(output.value, pool_nft.symbol, pool_nft.name) == 1
        },
      )

    let new_minted_lp =
      assets.quantity_of(
        self.mint,
        lp_token,
        // lp name == nft name
        pool_nft.name,
      )

    let
      in_a_amount,
      in_b_amount,
    <-
      collect_amounts(
        dict.to_pairs(assets.to_dict(own_input.output.value)),
        token_a,
        token_b,
      )

    let
      out_a_amount,
      out_b_amount,
    <-
      collect_amounts(
        dict.to_pairs(assets.to_dict(own_output.value)),
        token_a,
        token_b,
      )

    expect InlineDatum(output_datum) = own_output.datum
    expect (
      output_token_a,
      output_token_b,
      output_pool_nft,
      output_lp_token,
      output_minted_lp_tokens,
      output_swap_fee,
    ): Datum = output_datum

    let common_checks = and {
        (assets.quantity_of(
          own_input.output.value,
          pool_nft.symbol,
          pool_nft.name,
        ) == 1)?,
        (own_output.reference_script == None)?,
        (token_a == output_token_a)?,
        (token_b == output_token_b)?,
        (pool_nft == output_pool_nft)?,
        (lp_token == output_lp_token)?,
        (swap_fee == output_swap_fee)?,
      }

    when redeemer is {
      // Swap
      0 -> {
        let valid_lp = minted_lp_tokens == output_minted_lp_tokens
        let no_mint_lp = new_minted_lp == 0
        let swapped_not_too_much =
          check_swap(
            swap_fee,
            in_a_amount,
            in_b_amount,
            out_a_amount,
            out_b_amount,
          )
        valid_lp? && no_mint_lp? && swapped_not_too_much && common_checks?
      }
      // DepositLiquidity
      1 -> {
        let valid_mint_lp =
          minted_lp_tokens + new_minted_lp == output_minted_lp_tokens
        let no_burn_lp = new_minted_lp > 0
        let deposit_enough =
          output_minted_lp_tokens * output_minted_lp_tokens <= out_a_amount * out_b_amount

        valid_mint_lp? && no_burn_lp? && deposit_enough? && common_checks?
      }
      // WithdrawLiquidity
      2 -> {
        let valid_mint_lp =
          minted_lp_tokens + new_minted_lp == output_minted_lp_tokens
        let no_mint_lp = new_minted_lp < 0
        let withdrawn_not_too_much =
          output_minted_lp_tokens * output_minted_lp_tokens <= out_a_amount * out_b_amount

        valid_mint_lp? && no_mint_lp? && withdrawn_not_too_much? && common_checks?
      }
      _ -> fail
    }
  }

  else(_) {
    fail
  }
}
