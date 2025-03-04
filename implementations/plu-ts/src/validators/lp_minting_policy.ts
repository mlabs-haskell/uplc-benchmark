import { passert, PCurrencySymbol, perror, pfn, plet, pmakeUnit, pmatch, PScriptContext, PUnit, punsafeConvertType, PValue, TermFn, unit } from "@harmoniclabs/plu-ts";
import { policyValueOf } from "./utils/policyValueOf";


export const lpMintingPolicy: TermFn<[ typeof PCurrencySymbol, typeof PScriptContext ], PUnit> = pfn([
    PCurrencySymbol.type,
    PScriptContext.type
], unit)
(( poolNftSym, { tx, purpose } ) =>
    pmatch( purpose )
    .onMinting(({ currencySym: ownHash }) => passert.$(
        plet(
            policyValueOf.$( poolNftSym )
        ).in( amountOfPolicy =>
            tx.mint.some(({ fst: currencySym, snd: assets }) =>
                currencySym.eq( ownHash )
                .and(
                    assets.every(({ fst: lpName }) =>
                        tx.inputs.some(({ resolved: input }) =>
                            amountOfPolicy
                            .$( lpName )
                            .$(
                                // skip lovelaces
                                punsafeConvertType( input.value.tail, PValue.type )
                            ).eq( 1 )
                        )
                    )
                )
            )
        )
    ))
    ._(_ => perror( unit ))
)