#!opshin
from opshin.prelude import *
from opshin.std.builtins import *

# Opshin doesn't support any reasonable way of providing custom datum decoders
# so here we go.

NftMarketplaceRedeemer = int
NftMarketplaceRedeemerBuy = 0
NftMarketplaceRedeemerCancel = 1

NftMarketplaceDatum = List[Anything]


def datum_price(datum: NftMarketplaceDatum) -> Value:
    x: Value = datum[0]
    return x


def datum_address(datum: NftMarketplaceDatum) -> Address:
    x: Address = datum[1]
    return x


def datum_cancel_key(datum: NftMarketplaceDatum) -> PubKeyHash:
    x: PubKeyHash = datum[2]
    return x


def inline_datum_eq(datum: OutputDatum, expected: Anything) -> bool:
    if isinstance(datum, SomeOutputDatum):
        inline_datum: Datum = datum.datum
        return equals_data(inline_datum, expected)
    else:
        return False


def value_eq(lhs: Value, rhs: Value) -> bool:
    lhs_data: Anything = lhs
    rhs_data: Anything = rhs
    return equals_data(lhs_data, rhs_data)


def valid_output(
    output: TxOut, datum: NftMarketplaceDatum, own_input: TxOutRef
) -> bool:
    own_input_data: Anything = own_input
    return (
        output.address == datum_address(datum)
        and value_eq(output.value, datum_price(datum))
        and inline_datum_eq(output.datum, own_input_data)
    )


def validator(
    datum: NftMarketplaceDatum, redeemer: NftMarketplaceRedeemer, context: ScriptContext
) -> None:
    assert len(datum) == 3, "Invalid datum"

    purpose = context.purpose
    if isinstance(purpose, Spending):
        own_input = purpose.tx_out_ref
    else:
        assert False, "Invalid purpose"

    if redeemer == NftMarketplaceRedeemerBuy:
        has_valid_payment = any(
            [
                valid_output(output, datum, own_input)
                for output in context.tx_info.outputs
            ]
        )

        assert has_valid_payment
    elif redeemer == NftMarketplaceRedeemerCancel:
        is_signed = datum_cancel_key(datum) in context.tx_info.signatories
        assert is_signed
    else:
        assert False, "Invalid redeemer"
