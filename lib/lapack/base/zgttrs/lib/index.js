
'use strict';

/**
* Solve tridiagonal system using LU factorization (complex).
*
* @module @stdlib/lapack/base/zgttrs
*
*
* @example
* var zgttrs = require( '@stdlib/lapack/base/zgttrs' );
*
* var N = 3;
* var DU = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var DL = discreteUniform( N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var DU = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
*
* zgttrs.ndarray( 'no-transpose', N, N, DL, 1, 0, d, 1, 0, DU, 1, 0, 1, 1, 0, IPIV, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgttrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgttrs = main;
} else {
	zgttrs = tmp;
}


// EXPORTS //

module.exports = zgttrs;

// exports: { "ndarray": "zgttrs.ndarray" }
