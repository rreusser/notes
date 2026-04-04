
'use strict';

/**
* Computes the inverse of a real symmetric positive definite matrix in Rectangular Full Packed format.
*
* @module @stdlib/lapack/base/dpftri
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dpftri = require( '@stdlib/lapack/base/dpftri' );
*
* // 3x3 Cholesky-factored SPD matrix in RFP format (TRANSR='N', UPLO='L'):
* var A = new Float64Array( [ 3.162, 0.949, 0.316, 2.449, 2.646, 0.693 ] );
* var info = dpftri.ndarray( 'no-transpose', 'lower', 3, A, 1, 0 );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dpftri.ndarray" }
