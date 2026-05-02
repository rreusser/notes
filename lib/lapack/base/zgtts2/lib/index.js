
'use strict';

/**
* Solve tridiagonal system using LU factorization (complex).
*
* @module @stdlib/lapack/base/zgtts2
*
*
* @example
* var zgtts2 = require( '@stdlib/lapack/base/zgtts2' );
*
* var N = 3;
* var DU = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var DL = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var DU = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
*
* zgtts2.ndarray( 1, N, N, DL, 1, 0, d, 1, 0, DU, 1, 0, 1, 1, 0, IPIV, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgtts2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgtts2 = main;
} else {
	zgtts2 = tmp;
}


// EXPORTS //

module.exports = zgtts2;

// exports: { "ndarray": "zgtts2.ndarray" }
