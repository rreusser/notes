

'use strict';

/**
* Applies a complex unitary matrix Q from a QL factorization to a matrix (unblocked)
*
* @module @stdlib/lapack/base/zunm2l
*
*
* @example
* var zunm2l = require( '@stdlib/lapack/base/zunm2l' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zunm2l.ndarray( 'left', 'no-transpose', N, N, N, A, N, 1, 0, TAU, 1, 0, C, N, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zunm2l;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zunm2l = main;
} else {
	zunm2l = tmp;
}


// EXPORTS //

module.exports = zunm2l;

// exports: { "ndarray": "zunm2l.ndarray" }
