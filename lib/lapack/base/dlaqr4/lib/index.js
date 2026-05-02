
'use strict';

/**
* Multi-shift QR algorithm for eigenvalues of a Hessenberg matrix.
*
* @module @stdlib/lapack/base/dlaqr4
*
*
* @example
* var dlaqr4 = require( '@stdlib/lapack/base/dlaqr4' );
*
* var N = 3;
* var H = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var WR = discreteUniform( N, -10, 10, opts );
* var WI = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dlaqr4.ndarray( 1, 1, N, N, N, H, N, 1, 0, WR, 1, 0, WI, 1, 0, 1, 1, Z, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlaqr4;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqr4 = main;
} else {
	dlaqr4 = tmp;
}


// EXPORTS //

module.exports = dlaqr4;

// exports: { "ndarray": "dlaqr4.ndarray" }
