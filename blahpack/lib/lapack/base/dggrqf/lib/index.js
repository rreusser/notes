
'use strict';

/**
* Computes a generalized RQ factorization of an M-by-N matrix A and a P-by-N matrix B.
*
* @module @stdlib/lapack/base/dggrqf
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dggrqf = require( '@stdlib/lapack/base/dggrqf' );
*
* var A = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 4.0, 2.0, 3.0, 2.0, 5.0 ] );
* var B = new Float64Array( [ 1.0, 2.0, 1.0, 3.0, 1.0, 2.0, 2.0, 3.0, 1.0 ] );
* var TAUA = new Float64Array( 3 );
* var TAUB = new Float64Array( 3 );
*
* var info = dggrqf( 3, 3, 3, A, 3, TAUA, 1, B, 3, TAUB, 1 );
* // info => 0
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dggrqf = require( '@stdlib/lapack/base/dggrqf' );
*
* var A = new Float64Array( [ 2.0, 1.0, 3.0, 1.0, 4.0, 2.0, 3.0, 2.0, 5.0 ] );
* var B = new Float64Array( [ 1.0, 2.0, 1.0, 3.0, 1.0, 2.0, 2.0, 3.0, 1.0 ] );
* var TAUA = new Float64Array( 3 );
* var TAUB = new Float64Array( 3 );
*
* var info = dggrqf.ndarray( 3, 3, 3, A, 1, 3, 0, TAUA, 1, 0, B, 1, 3, 0, TAUB, 1, 0 );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dggrqf.ndarray" }
