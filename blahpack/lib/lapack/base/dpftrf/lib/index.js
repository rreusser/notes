
'use strict';

/**
* Computes the Cholesky factorization of a real symmetric positive definite matrix in Rectangular Full Packed format.
*
* @module @stdlib/lapack/base/dpftrf
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dpftrf = require( '@stdlib/lapack/base/dpftrf' );
*
* // 3x3 SPD matrix in RFP format (TRANSR='N', UPLO='L'):
* var A = new Float64Array( [ 10, 3, 1, 6, 8, 2 ] );
* var info = dpftrf( 'no-transpose', 'lower', 3, A, 1, 0 );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dpftrf.ndarray" }
