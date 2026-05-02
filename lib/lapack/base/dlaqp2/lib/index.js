
'use strict';

/**
* Computes a QR factorization with column pivoting using Level 2 BLAS.
*
* @module @stdlib/lapack/base/dlaqp2
*
*
* @example
* var dlaqp2 = require( '@stdlib/lapack/base/dlaqp2' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var JPVT = discreteUniform( N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dlaqp2.ndarray( N, N, 0, A, N, 1, 0, JPVT, 1, 0, TAU, 1, 0, 1, N, 0, 1, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaqp2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqp2 = main;
} else {
	dlaqp2 = tmp;
}


// EXPORTS //

module.exports = dlaqp2;

// exports: { "ndarray": "dlaqp2.ndarray" }
