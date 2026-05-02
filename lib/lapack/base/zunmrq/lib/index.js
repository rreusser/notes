

'use strict';

/**
* Multiplies a general matrix by the unitary matrix Q from an RQ factorization (blocked)
*
* @module @stdlib/lapack/base/zunmrq
*
*
* @example
* var zunmrq = require( '@stdlib/lapack/base/zunmrq' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zunmrq.ndarray( 'left', 'no-transpose', N, N, N, A, N, 1, 0, TAU, 1, 0, C, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zunmrq;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zunmrq = main;
} else {
	zunmrq = tmp;
}


// EXPORTS //

module.exports = zunmrq;

// exports: { "ndarray": "zunmrq.ndarray" }
