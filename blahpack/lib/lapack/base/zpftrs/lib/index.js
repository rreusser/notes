
'use strict';

/**
* Solves a complex system A * X = B where A is Hermitian positive definite in Rectangular Full Packed format.
*
* @module @stdlib/lapack/base/zpftrs
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpftrs = require( '@stdlib/lapack/base/zpftrs' );
*
* // 3x3 HPD matrix factored by zpftrf, in RFP (TRANSR='N', UPLO='L'):
* var A = new Complex128Array( [ 3.162, 0, 0.949, -0.316, 0.316, 0.632, 2.138, 0, 2.646, 0, 0.718, -0.643 ] );
* var B = new Complex128Array( [ 1, 0, 2, 0, 3, 0 ] );
* var info = zpftrs( 'no-transpose', 'lower', 3, 1, A, B );
* // info => 0
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpftrs = require( '@stdlib/lapack/base/zpftrs' );
*
* var A = new Complex128Array( [ 3.162, 0, 0.949, -0.316, 0.316, 0.632, 2.138, 0, 2.646, 0, 0.718, -0.643 ] );
* var B = new Complex128Array( [ 1, 0, 2, 0, 3, 0 ] );
* var info = zpftrs.ndarray( 'no-transpose', 'lower', 3, 1, A, 1, 0, B, 1, 3, 0 );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zpftrs.ndarray" }
