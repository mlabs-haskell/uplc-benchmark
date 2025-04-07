#!opshin
from opshin.prelude import *


@dataclass()
class AssetClass(PlutusData):
    CONSTR_ID = 0
    symbol: PolicyId
    name: TokenName


@dataclass()
class DexDatum(PlutusData):
    token_a: AssetClass
    token_b: AssetClass
    pool_nft: AssetClass
    lp_token: PolicyId
    minted_lp_tokens: int
    swap_fee: int


def decode_dex_datum(raw: List[Anything]) -> DexDatum:
    token_a: AssetClass = raw[0]
    token_b: AssetClass = raw[1]
    pool_nft: AssetClass = raw[2]
    lp_token: PolicyId = raw[3]
    minted_lp_tokens: int = raw[4]
    swap_fee: int = raw[5]
    return DexDatum(token_a, token_b, pool_nft, lp_token, minted_lp_tokens, swap_fee)


DexRedeemerSwap: int = 0
DexRedeemerDepositLiquidity: int = 1
DexRedeemerWithdrawLiquidity: int = 2


def quantity_of_asset(value: Value, asset_class: AssetClass) -> int:
    return value[asset_class.symbol][asset_class.name]


def quantity_of(value: Value, symbol: PolicyId, name: TokenName) -> int:
    if symbol in value.keys():
        return value[symbol].get(name, 0)
    else:
        return 0


def get_diff(new: int, old: int) -> int:
    if new - old < 0:
        return 0
    else:
        return new - old


# The uniswap fee is 0.3%; here it is multiplied by 1000, so that the
# on-chain code deals only in integers.
# See: <https://uniswap.org/whitepaper.pdf> Eq (11) (Page 7.)
fee_den: int = 1000


def check_swap(
    fee_num: int,
    old_a: int,
    old_b: int,
    new_a: int,
    new_b: int,
) -> bool:
    return fee_den * fee_den * old_a * old_b <= (
        new_a * fee_den - get_diff(new_a, old_a) * fee_num
    ) * (new_b * fee_den - get_diff(new_b, old_b) * fee_num)


def validator(
    input_datum_raw: List[Anything], redeemer: int, context: ScriptContext
) -> None:
    input_datum: DexDatum = decode_dex_datum(input_datum_raw)

    purpose = context.purpose
    if isinstance(purpose, Spending):
        own_input_ref = purpose.tx_out_ref
    else:
        assert False, "Invalid purpose"

    own_input = [inp for inp in context.tx_info.inputs if inp.out_ref == own_input_ref][
        0
    ]

    own_output = [
        out
        for out in context.tx_info.outputs
        if quantity_of_asset(out.value, input_datum.pool_nft) == 1
    ][0]

    # lp name == nft name
    new_minted_lp = quantity_of(
        context.tx_info.mint, input_datum.lp_token, input_datum.pool_nft.name
    )

    in_a_amount = quantity_of_asset(own_input.resolved.value, input_datum.token_a)
    in_b_amount = quantity_of_asset(own_input.resolved.value, input_datum.token_b)

    out_a_amount = quantity_of_asset(own_output.value, input_datum.token_a)
    out_b_amount = quantity_of_asset(own_output.value, input_datum.token_b)

    out_datum_some = own_output.datum
    if isinstance(out_datum_some, SomeOutputDatum):
        out_datum_raw: List[Anything] = out_datum_some.datum
    else:
        assert False, "Missing inline output datum"
    output_datum = decode_dex_datum(out_datum_raw)

    assert quantity_of_asset(own_input.resolved.value, input_datum.pool_nft) == 1
    own_output_reference_script = own_output.reference_script
    assert isinstance(own_output_reference_script, NoScriptHash)
    assert input_datum.token_a == output_datum.token_a
    assert input_datum.token_b == output_datum.token_b
    assert input_datum.pool_nft == output_datum.pool_nft
    assert input_datum.lp_token == output_datum.lp_token
    assert input_datum.swap_fee == output_datum.swap_fee

    if redeemer == DexRedeemerDepositLiquidity:
        assert (
            input_datum.minted_lp_tokens + new_minted_lp
            == output_datum.minted_lp_tokens
        )
        assert new_minted_lp > 0
        assert (
            output_datum.minted_lp_tokens * output_datum.minted_lp_tokens
            <= out_a_amount * out_b_amount
        )
    elif redeemer == DexRedeemerWithdrawLiquidity:
        assert (
            input_datum.minted_lp_tokens + new_minted_lp
            == output_datum.minted_lp_tokens
        )
        assert new_minted_lp < 0
        assert (
            output_datum.minted_lp_tokens * output_datum.minted_lp_tokens
            <= out_a_amount * out_b_amount
        )
    elif redeemer == DexRedeemerSwap:
        assert input_datum.minted_lp_tokens == output_datum.minted_lp_tokens
        assert new_minted_lp == 0
        assert check_swap(
            input_datum.swap_fee,
            in_a_amount,
            in_b_amount,
            out_a_amount,
            out_b_amount,
        )
    else:
        assert False, "Invalid redeemer"
