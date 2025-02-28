import { TermFn, PScriptContext, PUnit, pfn, unit, passert, pmatch, PTxOutRef, bool, perror, pisEmpty, plet, pshowInt } from "@harmoniclabs/plu-ts";

export const nftMintingPolicyValidator: TermFn<[
    typeof PScriptContext
], PUnit> = pfn([ PScriptContext.type ], unit)
(({ tx, redeemer, purpose }) => passert.$(
    plet(
        redeemer.as( PTxOutRef.type )
    ).in( mustSpendRef => 
        pmatch( purpose )
        .onMinting(({ currencySym: ownHash }) =>
            tx.mint.some(({ fst: currencySym, snd: assets }) =>
                currencySym.eq( ownHash )
                .and(
                    pisEmpty.$( assets.tail )
                    .strictAnd(
                        plet( assets.head ).in(({ fst: nftName, snd: qty }) =>
                            qty.eq( 1 )
                            .strictAnd(
                                nftName.eq( mustSpendRef.id.concat( pshowInt.$( mustSpendRef.index ) ) )
                            )
                            .strictAnd(
                                tx.inputs.some(({ utxoRef }) =>
                                    utxoRef.eq( mustSpendRef )
                                )
                            )
                        )
                    )
                )
            )
        )
        ._(_ => perror( bool ))
    )
))