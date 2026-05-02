
'use strict';

/**
* Multiplies a matrix by the orthogonal matrix Q from Hessenberg reduction.
*
* @module @stdlib/lapack/base/dormhr
*
*
* @example
* var dormhr = require( '@stdlib/lapack/base/dormhr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dormhr.ndarray( 'left', 'no-transpose', N, N, N, N, A, N, 1, 0, TAU, 1, 0, C, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dormhr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dormhr = main;
} else {
	dormhr = tmp;
}


// EXPORTS //

module.exports = dormhr;

// exports: { "ndarray": "dormhr.ndarray" }
