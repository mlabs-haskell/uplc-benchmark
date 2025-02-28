import { int, pfn, phoist, pstrictIf } from "@harmoniclabs/plu-ts";

export const positiveOrZero = phoist(
    pfn([ int ], int )
    ( n => pstrictIf( int ).$( n.lt( 0 ) ).$( 0 ).$( n ) )
)