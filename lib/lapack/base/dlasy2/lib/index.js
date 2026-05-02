
'use strict';

/**
* Solves the real Sylvester matrix equation for 1-by-1 or 2-by-2 matrices.
*
* @module @stdlib/lapack/base/dlasy2
*
*
* @example
* var dlasy2 = require( '@stdlib/lapack/base/dlasy2' );
*
* var N = 3;
* var TL = discreteUniform( N * N, -10, 10, opts );
* var TR = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var X = discreteUniform( N * N, -10, 10, opts );
*
* dlasy2.ndarray( 1, 1, 1, 1, 1, TL, N, 1, 0, TR, N, 1, 0, B, N, 1, 0, 1.0, X, N, 1, 0, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dlasy2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlasy2 = main;
} else {
	dlasy2 = tmp;
}


// EXPORTS //

module.exports = dlasy2;

// exports: { "ndarray": "dlasy2.ndarray" }
