
'use strict';

/**
* Computes the generalized singular value decomposition of a real matrix pair.
*
* @module @stdlib/lapack/base/dggsvd3
*
*
* @example
* var dggsvd3 = require( '@stdlib/lapack/base/dggsvd3' );
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
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dggsvd3.ndarray( 'compute-U', 'compute-V', 'compute-Q', N, N, 1, N, 1, A, N, 1, 0, B, N, 1, 0, ALPHA, 1, 0, BETA, 1, 0, U, N, 1, 0, V, N, 1, 0, Q, N, 1, 0, WORK, 1, 0, N, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dggsvd3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dggsvd3 = main;
} else {
	dggsvd3 = tmp;
}


// EXPORTS //

module.exports = dggsvd3;

// exports: { "ndarray": "dggsvd3.ndarray" }
