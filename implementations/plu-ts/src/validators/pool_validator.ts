import { PCurrencySymbol, PScriptContext, pstruct, PTokenName, unit, pfn, passert, int, pInt, TermInt, TermBool, bool, perror, pmatch, plet, punsafeConvertType, PMaybe, PTxInInfo, PTxOut, PUnit, TermFn, PValue } from "@harmoniclabs/plu-ts"
import { positiveOrZero } from "./utils/positiveOrZero";
import { policyValueOf } from "./utils/policyValueOf";

const AssetClass = pstruct({
    AssetClass: {
        symbol: PCurrencySymbol.type,
        tokenName: PTokenName.type
    }
})

const DexDatum = pstruct({
    DexDatum: {
        token_a: AssetClass.type,
        token_b: AssetClass.type,
        poolNft: AssetClass.type,
        lpToken: PCurrencySymbol.type,
        mintedLps: int,
        swapFee: int
    }
});

const DexRdmr = pstruct({
    Swap: {},
    Deposit: {},
    Withdraw: {}
});

// The uniswap fee is 0.3%; here it is multiplied by 1000, so that the
// on-chain code deals only in integers.
// See: <https://uniswap.org/whitepaper.pdf> Eq (11) (Page 7.)
const feeDenom = pInt( 1000 );
// this will be optimized anyway, just save some compilation time
const feeDenomSquared = pInt( 1000 * 1000 );

export const poolValidator: TermFn<[typeof PScriptContext], PUnit> = pfn([ PScriptContext.type ], unit)
(({ tx, redeemer, purpose }) => passert.$(
    pmatch( purpose )
    .onSpending(({ utxoRef: spendingRef, datum: maybeDatum }) => {

        const inputDatum = plet(
            maybeDatum.unwrap.as( DexDatum.type )
        );

        const ownIn = plet(
            // just helping typescript a little
            punsafeConvertType(
                tx.inputs.find(({ utxoRef }) => utxoRef.eq( spendingRef ) ),
                PMaybe( PTxInInfo.type ).type
            ).unwrap.resolved
        );
        const ownValue = plet(
            punsafeConvertType( ownIn.value.tail, PValue.type )
        );

        const ownOut = plet(
            plet(
                policyValueOf.$( inputDatum.poolNft.symbol ).$( inputDatum.poolNft.tokenName )
            ).in( amountOfPoolNft =>
                punsafeConvertType(
                    tx.outputs.find( out => amountOfPoolNft.$( out.value.tail ).eq( 1 ) ),
                    PMaybe( PTxOut.type ).type
                ).unwrap
            )
        );

        const newMintedLp = plet(
            tx.mint.amountOf( inputDatum.lpToken, inputDatum.poolNft.tokenName )
        );

        const getTokenA = plet(
            policyValueOf.$( inputDatum.token_a.symbol ).$( inputDatum.token_a.tokenName )
        );
        const getTokenB = plet(
            policyValueOf.$( inputDatum.token_b.symbol ).$( inputDatum.token_b.tokenName )
        );

        const in_a_amt = plet(
            getTokenA
            .$( ownValue )
        );
        const in_b_amt = plet(
            getTokenB
            .$( ownValue )
        );

        const out_a_amt = plet(
            getTokenA
            .$( ownOut.value.tail )
        );
        const out_b_amt = plet(
            getTokenB
            .$( ownOut.value.tail )
        );

        const outDatum = plet(
            // fails if not inline
            ownOut.datum.raw.fields.head.as( DexDatum.type )
        );

        // inlined
        const inputHasNft = ownValue.amountOf( inputDatum.poolNft.symbol, inputDatum.poolNft.tokenName ).eq( 1 );
        // inlined
        const noOutRefScript = ownOut.datum.raw.index.eq( 1 ); // Nothing
        // inlined
        const sameA = inputDatum.token_a.eq( outDatum.token_a );
        // inlined
        const sameB = inputDatum.token_b.eq( outDatum.token_b );
        // inlined
        const sameNft = inputDatum.poolNft.eq( outDatum.poolNft );
        // inlined
        const sameLp = inputDatum.lpToken.eq( outDatum.lpToken );
        // inlined
        const sameFee = inputDatum.swapFee.eq( outDatum.swapFee );

        return inputHasNft
        .strictAnd( noOutRefScript )
        .strictAnd( sameA )
        .strictAnd( sameB )
        .strictAnd( sameNft )
        .strictAnd( sameLp )
        .strictAnd( sameFee )
        .strictAnd(
            pmatch( redeemer.as( DexRdmr.type ) )
            .onDeposit(() =>
                inputDatum.mintedLps.add( newMintedLp ).eq( outDatum.mintedLps )
                .strictAnd( newMintedLp.gt( 0 ) )
                .strictAnd(
                    outDatum.mintedLps.mult( outDatum.mintedLps ).ltEq(
                        out_a_amt.mult( out_b_amt )
                    )
                )
            )
            .onWithdraw(() =>
                inputDatum.mintedLps.add( newMintedLp ).eq( outDatum.mintedLps )
                .strictAnd( newMintedLp.lt( 0 ) )
                .strictAnd(
                    outDatum.mintedLps.mult( outDatum.mintedLps ).ltEq(
                        out_a_amt.mult( out_b_amt )
                    )
                )
            )
            .onSwap(() =>
                inputDatum.mintedLps.eq( outDatum.mintedLps )
                .strictAnd( newMintedLp.eq( 0 ) )
                .strictAnd(
                    inline_checkSwap(
                        inputDatum.swapFee,
                        in_a_amt,
                        in_b_amt,
                        out_a_amt,
                        out_b_amt
                    )
                )
            )
        ) 
    }
    )
    ._(_ => perror( bool ))
))

function inline_checkSwap(
    feeNum: TermInt,
    old_a: TermInt,
    old_b: TermInt,
    new_a: TermInt,
    new_b: TermInt
): TermBool
{
    /*
    fee_den * fee_den * old_a * old_b <= (
        new_a * fee_den - get_diff(new_a, old_a) * fee_num
    ) * ( new_b * fee_den - get_diff(new_b, old_b) * fee_num )
    */
    return feeDenomSquared.mult( old_a ).mult( old_b ).ltEq(
        feeDenom.mult( new_a ).sub( positiveOrZero.$( new_a.sub( old_a ) ).mult( feeNum ) ).mult(
            feeDenom.mult( new_b ).sub( positiveOrZero.$( new_b.sub( old_b ) ).mult( feeNum ) )
        )
    )
}