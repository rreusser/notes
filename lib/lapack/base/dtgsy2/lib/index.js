
'use strict';

/**
* Solves the generalized Sylvester equation (unblocked).
*
* @module @stdlib/lapack/base/dtgsy2
*
*
* @example
* var dtgsy2 = require( '@stdlib/lapack/base/dtgsy2' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
* var D = discreteUniform( N * N, -10, 10, opts );
* var E = discreteUniform( N * N, -10, 10, opts );
* var F = discreteUniform( N * N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dtgsy2.ndarray( 'no-transpose', 1, N, N, A, N, 1, 0, B, N, 1, 0, C, N, 1, 0, D, N, 1, 0, E, N, 1, 0, F, N, 1, 0, 1.0, 1, 1, IWORK, 1, 0, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtgsy2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtgsy2 = main;
} else {
	dtgsy2 = tmp;
}


// EXPORTS //

module.exports = dtgsy2;

// exports: { "ndarray": "dtgsy2.ndarray" }
