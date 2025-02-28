import { bool, data, int, PAddress, passert, peqData, perror, pfn, plet, pmatch, PPubKeyHash, PScriptContext, pstruct, PUnit, PValue, TermFn, unit } from "@harmoniclabs/plu-ts";

const NftMarketplaceDatum = pstruct({
    NftMarketplaceDatum: {
        price: PValue.type,
        seller: PAddress.type,
        cancel_key: PPubKeyHash.type
    }
});

const Rdmr = pstruct({
    Buy: {},
    Cancel: {}
});

export const nftMarketplaceValidator: TermFn<[ typeof PScriptContext ], PUnit> = pfn([ PScriptContext.type ], unit)
(({ tx, redeemer, purpose }) => passert.$(
    pmatch( purpose )
    .onSpending(({ utxoRef: spendingRef, datum: maybeDatum }) => {
        const datum = plet(
            maybeDatum.unwrap.as( NftMarketplaceDatum.type )
        );

        // inlined (used once)
        const ownIn = (
            plet(
                tx.inputs.find(({ utxoRef }) => utxoRef.eq( spendingRef ))
            ).unwrap
        );

        return pmatch( redeemer.as( Rdmr.type ) )
        .onBuy(() => 
            tx.outputs.some(out =>
                peqData
                // inline or fail
                .$( out.datum.raw.fields.head )
                .$( ownIn.as( data ) )
                .strictAnd(
                    out.address.eq( datum.seller )
                )
                .strictAnd(
                    // check prices using builtin comparison
                    peqData
                    .$( out.raw.fields.tail.head )
                    .$( datum.raw.fields.head )
                )
            )
        )
        .onCancel(() =>
            tx.signatories.some( datum.cancel_key.peq )
        )
    })
    ._(_ => perror( bool )) as any
))