
'use strict';

/**
* Computes eigenvalues and Schur decomposition of an upper Hessenberg matrix.
*
* @module @stdlib/lapack/base/dhseqr
*
*
* @example
* var dhseqr = require( '@stdlib/lapack/base/dhseqr' );
*
* var N = 3;
* var H = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var WR = discreteUniform( N, -10, 10, opts );
* var WI = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dhseqr.ndarray( 'both', 'update', N, N, N, H, N, 1, 0, WR, 1, 0, WI, 1, 0, Z, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dhseqr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dhseqr = main;
} else {
	dhseqr = tmp;
}


// EXPORTS //

module.exports = dhseqr;

// exports: { "ndarray": "dhseqr.ndarray" }
