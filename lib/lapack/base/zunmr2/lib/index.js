

'use strict';

/**
* Multiplies a general matrix by the unitary matrix Q from an RQ factorization (unblocked)
*
* @module @stdlib/lapack/base/zunmr2
*
*
* @example
* var zunmr2 = require( '@stdlib/lapack/base/zunmr2' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zunmr2.ndarray( 'left', 'no-transpose', N, N, N, A, N, 1, 0, TAU, 1, 0, C, N, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zunmr2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zunmr2 = main;
} else {
	zunmr2 = tmp;
}


// EXPORTS //

module.exports = zunmr2;

// exports: { "ndarray": "zunmr2.ndarray" }
