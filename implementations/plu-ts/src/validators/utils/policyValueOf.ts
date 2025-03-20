import { int, lam, PAssetsEntry, PCurrencySymbol, pdelay, pfn, phoist, pif, pInt, precursiveList, PTokenName, PValue, PValueEntry } from "@harmoniclabs/plu-ts";

/**
 * exactly like `value.amountOf`
 * 
 * but different order of parameters, to steal a little budget using partial function application
 */
export const policyValueOf = phoist(
    pfn([ PCurrencySymbol.type, PTokenName.type ], lam( PValue.type, int ))
    (( sym, tokenName ) =>
        precursiveList( int, PValueEntry.type )
        .$( _self => pdelay( pInt( 0 ) ) )
        .$((self, head, tail) =>
            pif( int ).$( head.fst.eq( sym ) )
            .then(
                precursiveList( int, PAssetsEntry.type )
                .$( _self => pdelay( pInt( 0 ) ) )
                .$((self, head, tail) =>
                    pif( int ).$( head.fst.eq( tokenName ) )
                    .then( head.snd )
                    .else( self.$( tail ) )
                )
                .$( head.snd )
            )
            .else( self.$( tail ) )
        )
        // .$( value )
    )
)