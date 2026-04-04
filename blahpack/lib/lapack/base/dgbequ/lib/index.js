
'use strict';

/**
* Computes row and column scalings to equilibrate a real general band matrix.
*
* @module @stdlib/lapack/base/dgbequ
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dgbequ = require( '@stdlib/lapack/base/dgbequ' );
*
* var AB = new Float64Array( [ 2, 1, 4, 6 ] );
* var r = new Float64Array( 2 );
* var c = new Float64Array( 2 );
* var out = dgbequ.ndarray( 2, 2, 0, 0, AB, 1, 1, 0, r, 1, 0, c, 1, 0 );
* // out.info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dgbequ.ndarray" }
