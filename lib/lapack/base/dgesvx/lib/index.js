

'use strict';

/**
* Expert driver for solving a general system with equilibration and condition estimation
*
* @module @stdlib/lapack/base/dgesvx
*
*
* @example
* var dgesvx = require( '@stdlib/lapack/base/dgesvx' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AF = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var X = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var r = discreteUniform( N, -10, 10, opts );
* var c = discreteUniform( N, -10, 10, opts );
* var FERR = discreteUniform( N, -10, 10, opts );
* var BERR = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dgesvx.ndarray( 'not-factored', 'no-transpose', N, N, A, N, 1, 0, AF, N, 1, 0, IPIV, 1, 0, 'none', r, 1, 0, c, 1, 0, B, N, 1, 0, X, N, 1, 0, 1.0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgesvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgesvx = main;
} else {
	dgesvx = tmp;
}


// EXPORTS //

module.exports = dgesvx;

// exports: { "ndarray": "dgesvx.ndarray" }
