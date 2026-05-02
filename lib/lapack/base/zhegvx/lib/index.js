

'use strict';

/**
* Computes selected eigenvalues and eigenvectors of a complex generalized Hermitian-definite eigenproblem
*
* @module @stdlib/lapack/base/zhegvx
*
*
* @example
* var zhegvx = require( '@stdlib/lapack/base/zhegvx' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
* var IFAIL = discreteUniform( N, -10, 10, opts );
*
* zhegvx.ndarray( N, 'compute-vectors', 'all', 'upper', N, A, N, 1, 0, B, N, 1, 0, 1.0, 1.0, N, N, 1.0, 1, w, 1, 0, Z, N, 1, 0, WORK, 1, 0, N, RWORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhegvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhegvx = main;
} else {
	zhegvx = tmp;
}


// EXPORTS //

module.exports = zhegvx;

// exports: { "ndarray": "zhegvx.ndarray" }
