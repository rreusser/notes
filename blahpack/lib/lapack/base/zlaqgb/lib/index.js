'use strict';

/**
* Equilibrates a complex general band matrix using row and column scaling factors.
*
* @module @stdlib/lapack/base/zlaqgb
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var zlaqgb = require( '@stdlib/lapack/base/zlaqgb' );
*
* // 2x2 band matrix with kl=1, ku=0, ldab=2:
* var AB = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0, 4.0, 2.0 ] );
* var r = new Float64Array( [ 0.5, 2.0 ] );
* var c = new Float64Array( [ 1.0, 1.0 ] );
*
* var equed = zlaqgb.ndarray( 2, 2, 1, 0, AB, 1, 2, 0, r, 1, 0, c, 1, 0, 0.01, 1.0, 5.0 );
* // returns 'row'
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zlaqgb.ndarray" }
