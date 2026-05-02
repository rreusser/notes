
'use strict';

/**
* Solves a real tridiagonal system using LU factorization from dgttrf.
*
* @module @stdlib/lapack/base/dgttrs
*
*
* @example
* var dgttrs = require( '@stdlib/lapack/base/dgttrs' );
*
* var N = 3;
* var DU = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var DL = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var DU = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
*
* dgttrs.ndarray( 'no-transpose', N, N, DL, 1, 0, d, 1, 0, DU, 1, 0, 1, 1, 0, IPIV, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgttrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgttrs = main;
} else {
	dgttrs = tmp;
}


// EXPORTS //

module.exports = dgttrs;

// exports: { "ndarray": "dgttrs.ndarray" }
