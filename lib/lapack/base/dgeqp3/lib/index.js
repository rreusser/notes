
'use strict';

/**
* Computes a QR factorization with column pivoting of a real matrix.
*
* @module @stdlib/lapack/base/dgeqp3
*
*
* @example
* var dgeqp3 = require( '@stdlib/lapack/base/dgeqp3' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var JPVT = discreteUniform( N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dgeqp3.ndarray( N, N, A, N, 1, 0, JPVT, 1, 0, TAU, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgeqp3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgeqp3 = main;
} else {
	dgeqp3 = tmp;
}


// EXPORTS //

module.exports = dgeqp3;

// exports: { "ndarray": "dgeqp3.ndarray" }
