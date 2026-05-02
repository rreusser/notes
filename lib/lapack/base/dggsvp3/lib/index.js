
'use strict';

/**
* Compute the preprocessing for the generalized SVD of real matrices A and B.
*
* @module @stdlib/lapack/base/dggsvp3
*
*
* @example
* var dggsvp3 = require( '@stdlib/lapack/base/dggsvp3' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var U = discreteUniform( N * N, -10, 10, opts );
* var V = discreteUniform( N * N, -10, 10, opts );
* var Q = discreteUniform( N * N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dggsvp3.ndarray( 'compute-U', 'compute-V', 'compute-Q', N, 1, N, A, N, 1, 0, B, N, 1, 0, 1, 1, N, 1, U, N, 1, 0, V, N, 1, 0, Q, N, 1, 0, IWORK, 1, 0, TAU, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dggsvp3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dggsvp3 = main;
} else {
	dggsvp3 = tmp;
}


// EXPORTS //

module.exports = dggsvp3;

// exports: { "ndarray": "dggsvp3.ndarray" }
