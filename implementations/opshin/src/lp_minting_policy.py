#!opshin
from opshin.prelude import *


def has_nft(
    pool_nft_cs: PolicyId, pool_nft_tn: TokenName, tx_in_info: TxInInfo
) -> bool:
    return tx_in_info.resolved.value[pool_nft_cs][pool_nft_tn] == 1


def validator(
    pool_nft_cs_raw: Anything, redeemer: Anything, context: ScriptContext
) -> None:
    pool_nft_cs: PolicyId = pool_nft_cs_raw
    purpose = context.purpose
    if isinstance(purpose, Minting):
        own_symbol = purpose.policy_id
    else:
        assert False, "Invalid purpose"

    minted_names = context.tx_info.mint[own_symbol].keys()
    assert all(
        [
            any(
                [
                    has_nft(pool_nft_cs, minted_lp_name, inp)
                    for inp in context.tx_info.inputs
                ]
            )
            for minted_lp_name in minted_names
        ]
    )
