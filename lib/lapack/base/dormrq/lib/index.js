
'use strict';

/**
* Multiply a matrix by the orthogonal matrix Q from RQ factorization (blocked).
*
* @module @stdlib/lapack/base/dormrq
*
*
* @example
* var dormrq = require( '@stdlib/lapack/base/dormrq' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dormrq.ndarray( 'left', 'no-transpose', N, N, N, A, N, 1, 0, TAU, 1, 0, C, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dormrq;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dormrq = main;
} else {
	dormrq = tmp;
}


// EXPORTS //

module.exports = dormrq;

// exports: { "ndarray": "dormrq.ndarray" }
