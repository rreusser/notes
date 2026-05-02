
'use strict';

/**
* Multiply a matrix by the orthogonal matrix Q from QL factorization (unblocked).
*
* @module @stdlib/lapack/base/dorm2l
*
*
* @example
* var dorm2l = require( '@stdlib/lapack/base/dorm2l' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dorm2l.ndarray( 'left', 'no-transpose', N, N, N, A, N, 1, 0, TAU, 1, 0, C, N, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dorm2l;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dorm2l = main;
} else {
	dorm2l = tmp;
}


// EXPORTS //

module.exports = dorm2l;

// exports: { "ndarray": "dorm2l.ndarray" }
