
'use strict';

/**
* Multiply a matrix by the orthogonal matrix Q from RQ factorization (unblocked).
*
* @module @stdlib/lapack/base/dormr2
*
*
* @example
* var dormr2 = require( '@stdlib/lapack/base/dormr2' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dormr2.ndarray( 'left', 'no-transpose', N, N, N, A, N, 1, 0, TAU, 1, 0, C, N, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dormr2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dormr2 = main;
} else {
	dormr2 = tmp;
}


// EXPORTS //

module.exports = dormr2;

// exports: { "ndarray": "dormr2.ndarray" }
