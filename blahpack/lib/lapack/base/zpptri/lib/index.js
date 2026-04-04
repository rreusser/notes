
'use strict';

/**
* Computes the inverse of a complex Hermitian positive definite matrix in packed storage using the Cholesky factorization.
*
* @module @stdlib/lapack/base/zpptri
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpptri = require( '@stdlib/lapack/base/zpptri' );
*
* // 2x2 upper Cholesky factor (already factored):
* var AP = new Complex128Array( [ 3.0, 0.0, 1.0, 0.0, 2.0, 0.0 ] );
*
* var info = zpptri( 'upper', 2, AP );
* // info => 0
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zpptri = require( '@stdlib/lapack/base/zpptri' );
*
* // 2x2 upper Cholesky factor (already factored):
* var AP = new Complex128Array( [ 3.0, 0.0, 1.0, 0.0, 2.0, 0.0 ] );
*
* var info = zpptri.ndarray( 'upper', 2, AP, 1, 0 );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zpptri.ndarray" }
