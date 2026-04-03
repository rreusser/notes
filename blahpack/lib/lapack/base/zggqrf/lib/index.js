
'use strict';

/**
* Computes a generalized QR factorization of an N-by-M matrix A and an N-by-P matrix B.
*
* @module @stdlib/lapack/base/zggqrf
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zggqrf = require( '@stdlib/lapack/base/zggqrf' );
*
* var A = new Complex128Array( [ 5, 2 ] );
* var TAUA = new Complex128Array( 1 );
* var B = new Complex128Array( [ 3, -1 ] );
* var TAUB = new Complex128Array( 1 );
* var WORK = new Complex128Array( 64 );
*
* var info = zggqrf.ndarray( 1, 1, 1, A, 1, 1, 0, TAUA, 1, 0, B, 1, 1, 0, TAUB, 1, 0, WORK, 1, 0, 64 );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zggqrf.ndarray" }
