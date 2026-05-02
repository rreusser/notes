
'use strict';

/**
* Computes the minimum norm solution to a complex linear least squares problem using SVD.
*
* @module @stdlib/lapack/base/zgelss
*
*
* @example
* var zgelss = require( '@stdlib/lapack/base/zgelss' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var S = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
*
* zgelss.ndarray( N, N, N, A, N, 1, 0, B, N, 1, 0, S, 1, 0, 1.0, 1, WORK, 1, 0, N, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgelss;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgelss = main;
} else {
	zgelss = tmp;
}


// EXPORTS //

module.exports = zgelss;

// exports: { "ndarray": "zgelss.ndarray" }
