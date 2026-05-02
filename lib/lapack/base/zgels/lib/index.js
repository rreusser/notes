
'use strict';

/**
* Solve complex linear least squares using QR or LQ factorization.
*
* @module @stdlib/lapack/base/zgels
*
*
* @example
* var zgels = require( '@stdlib/lapack/base/zgels' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zgels.ndarray( 'no-transpose', N, N, N, A, N, 1, 0, B, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgels;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgels = main;
} else {
	zgels = tmp;
}


// EXPORTS //

module.exports = zgels;

// exports: { "ndarray": "zgels.ndarray" }
