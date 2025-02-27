#!opshin
from opshin.prelude import *


def int_to_bs(n: int) -> List[int]:
    acc: List[int] = []
    while n != 0:
        if n < 256:
            acc = acc + [n]
            n = 0
        else:
            acc = acc + [n % 256]
            n = n // 256
    return acc


def derive_nft_name(ref: TxOutRef) -> TokenName:
    return ref.id.tx_id + bytes(int_to_bs(ref.idx))


def validator(initial_spend: TxOutRef, context: ScriptContext) -> None:
    purpose = context.purpose
    if isinstance(purpose, Minting):
        own_symbol = purpose.policy_id
    else:
        assert False, "Invalid purpose"

    minted_names = context.tx_info.mint[own_symbol].items()
    assert len(minted_names) == 1, "Minted ore than one NFT name"
    minted_nft_name, minted_nft_amount = minted_names[0]

    assert minted_nft_amount == 1, "Minted one NFT"
    assert minted_nft_name == derive_nft_name(
        initial_spend
    ), "Minted NFT name does not match derived name"

    assert any(
        [initial_spend == inp.out_ref for inp in context.tx_info.inputs]
    ), "Initial spent is not spent"
