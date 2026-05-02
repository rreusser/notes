
'use strict';

/**
* Solves a complex banded system of linear equations A*X = B using LU factorization.
*
* @module @stdlib/lapack/base/zgbsv
*
*
* @example
* var zgbsv = require( '@stdlib/lapack/base/zgbsv' );
*
* var N = 3;
* var AB = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
*
* zgbsv.ndarray( N, N, N, N, AB, N, 1, 0, IPIV, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgbsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgbsv = main;
} else {
	zgbsv = tmp;
}


// EXPORTS //

module.exports = zgbsv;

// exports: { "ndarray": "zgbsv.ndarray" }
