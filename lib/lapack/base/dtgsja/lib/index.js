
'use strict';

/**
* Computes the generalized SVD of two upper triangular matrices via Jacobi-Kogbetliantz iteration.
*
* @module @stdlib/lapack/base/dtgsja
*
*
* @example
* var dtgsja = require( '@stdlib/lapack/base/dtgsja' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var U = discreteUniform( N * N, -10, 10, opts );
* var V = discreteUniform( N * N, -10, 10, opts );
* var Q = discreteUniform( N * N, -10, 10, opts );
* var ALPHA = discreteUniform( N, -10, 10, opts );
* var BETA = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dtgsja.ndarray( 'compute-U', 'compute-V', 'compute-Q', N, 1, N, N, 1, A, N, 1, 0, B, N, 1, 0, 1, 1, ALPHA, 1, 0, BETA, 1, 0, U, N, 1, 0, V, N, 1, 0, Q, N, 1, 0, WORK, 1, 0, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtgsja;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtgsja = main;
} else {
	dtgsja = tmp;
}


// EXPORTS //

module.exports = dtgsja;

// exports: { "ndarray": "dtgsja.ndarray" }
