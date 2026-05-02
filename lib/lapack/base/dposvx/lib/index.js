
'use strict';

/**
* Expert driver for symmetric positive definite solve with equilibration, condition estimation, and refinement.
*
* @module @stdlib/lapack/base/dposvx
*
*
* @example
* var dposvx = require( '@stdlib/lapack/base/dposvx' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var AF = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var X = discreteUniform( N * N, -10, 10, opts );
* var s = discreteUniform( N, -10, 10, opts );
* var FERR = discreteUniform( N, -10, 10, opts );
* var BERR = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dposvx.ndarray( 'not-factored', 'upper', N, N, A, N, 1, 0, AF, N, 1, 0, 'none', s, 1, 0, B, N, 1, 0, X, N, 1, 0, 1.0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dposvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dposvx = main;
} else {
	dposvx = tmp;
}


// EXPORTS //

module.exports = dposvx;

// exports: { "ndarray": "dposvx.ndarray" }
