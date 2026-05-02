

'use strict';

/**
* Compute eigenvalues and Schur form of complex upper Hessenberg matrix
*
* @module @stdlib/lapack/base/zhseqr
*
*
* @example
* var zhseqr = require( '@stdlib/lapack/base/zhseqr' );
*
* var N = 3;
* var H = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zhseqr.ndarray( 'both', 'update', N, N, N, H, N, 1, 0, w, 1, 0, Z, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhseqr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhseqr = main;
} else {
	zhseqr = tmp;
}


// EXPORTS //

module.exports = zhseqr;

// exports: { "ndarray": "zhseqr.ndarray" }
