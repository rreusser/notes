
'use strict';

/**
* Computes a step of QR factorization with column pivoting using Level 3 BLAS.
*
* @module @stdlib/lapack/base/dlaqps
*
*
* @example
* var dlaqps = require( '@stdlib/lapack/base/dlaqps' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var F = discreteUniform( N * N, -10, 10, opts );
* var JPVT = discreteUniform( N, -10, 10, opts );
* var TAU = discreteUniform( N, -10, 10, opts );
* var AUXV = discreteUniform( N, -10, 10, opts );
*
* dlaqps.ndarray( N, N, 0, N, N, A, N, 1, 0, JPVT, 1, 0, TAU, 1, 0, 1, N, 0, 1, 1, 0, AUXV, 1, 0, F, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaqps;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqps = main;
} else {
	dlaqps = tmp;
}


// EXPORTS //

module.exports = dlaqps;

// exports: { "ndarray": "dlaqps.ndarray" }
