
'use strict';

/**
* Solves a system of linear equations `A * X = B` with a symmetric positive definite matrix in Rectangular Full Packed format.
*
* @module @stdlib/lapack/base/dpftrs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dpftrs = require( '@stdlib/lapack/base/dpftrs' );
*
* // 3x3 SPD matrix (Cholesky-factored) in RFP format (TRANSR='N', UPLO='L'):
* var A = new Float64Array( [ 3.0, 1.0, 0.5, 2.0, 2.5, 1.0 ] );
* var B = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var info = dpftrs.ndarray( 'no-transpose', 'lower', 3, 1, A, 1, 0, B, 1, 3, 0 );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dpftrs.ndarray" }
