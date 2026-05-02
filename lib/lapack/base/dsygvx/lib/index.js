
'use strict';

/**
* Computes selected eigenvalues and eigenvectors of a real generalized symmetric-definite eigenproblem.
*
* @module @stdlib/lapack/base/dsygvx
*
*
* @example
* var dsygvx = require( '@stdlib/lapack/base/dsygvx' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
* var IFAIL = discreteUniform( N, -10, 10, opts );
*
* dsygvx.ndarray( N, 'compute-vectors', 'all', 'upper', N, A, N, 1, 0, B, N, 1, 0, 1.0, 1.0, N, N, 1.0, 1, w, 1, 0, Z, N, 1, 0, WORK, 1, 0, N, IWORK, 1, 0, IFAIL, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsygvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsygvx = main;
} else {
	dsygvx = tmp;
}


// EXPORTS //

module.exports = dsygvx;

// exports: { "ndarray": "dsygvx.ndarray" }
