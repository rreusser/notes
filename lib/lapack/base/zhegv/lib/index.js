

'use strict';

/**
* Computes all eigenvalues and optionally eigenvectors of a complex generalized Hermitian-definite eigenproblem
*
* @module @stdlib/lapack/base/zhegv
*
*
* @example
* var zhegv = require( '@stdlib/lapack/base/zhegv' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
*
* zhegv.ndarray( N, 'compute', 'upper', N, A, N, 1, 0, B, N, 1, 0, w, 1, 0, WORK, 1, 0, N, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhegv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhegv = main;
} else {
	zhegv = tmp;
}


// EXPORTS //

module.exports = zhegv;

// exports: { "ndarray": "zhegv.ndarray" }
