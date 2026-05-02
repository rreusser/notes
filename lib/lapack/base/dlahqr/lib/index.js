
'use strict';

/**
* Computes eigenvalues and Schur form of an upper Hessenberg matrix (small/medium).
*
* @module @stdlib/lapack/base/dlahqr
*
*
* @example
* var dlahqr = require( '@stdlib/lapack/base/dlahqr' );
*
* var N = 3;
* var H = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var WR = discreteUniform( N, -10, 10, opts );
* var WI = discreteUniform( N, -10, 10, opts );
*
* dlahqr.ndarray( 1, 1, N, N, N, H, N, 1, 0, WR, 1, 0, WI, 1, 0, 1, 1, Z, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlahqr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlahqr = main;
} else {
	dlahqr = tmp;
}


// EXPORTS //

module.exports = dlahqr;

// exports: { "ndarray": "dlahqr.ndarray" }
