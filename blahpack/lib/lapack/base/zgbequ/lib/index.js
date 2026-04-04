
'use strict';

/**
* Computes row and column scalings to equilibrate a complex general band matrix.
*
* @module @stdlib/lapack/base/zgbequ
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zgbequ = require( '@stdlib/lapack/base/zgbequ' );
*
* var AB = new Complex128Array( [ 3, 4, 1, 0, 0, 2 ] );
* var r = new Float64Array( 3 );
* var c = new Float64Array( 3 );
* var out = zgbequ.ndarray( 3, 3, 0, 0, AB, 1, 1, 0, r, 1, 0, c, 1, 0 );
* // out.info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zgbequ.ndarray" }
