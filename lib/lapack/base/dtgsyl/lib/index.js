
'use strict';

/**
* Solves the generalized Sylvester equation (blocked).
*
* @module @stdlib/lapack/base/dtgsyl
*
*
* @example
* var dtgsyl = require( '@stdlib/lapack/base/dtgsyl' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var C = discreteUniform( N * N, -10, 10, opts );
* var D = discreteUniform( N * N, -10, 10, opts );
* var E = discreteUniform( N * N, -10, 10, opts );
* var F = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dtgsyl.ndarray( 'no-transpose', 1, N, N, A, N, 1, 0, B, N, 1, 0, C, N, 1, 0, D, N, 1, 0, E, N, 1, 0, F, N, 1, 0, 1.0, 1, WORK, 1, 0, N, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtgsyl;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtgsyl = main;
} else {
	dtgsyl = tmp;
}


// EXPORTS //

module.exports = dtgsyl;

// exports: { "ndarray": "dtgsyl.ndarray" }
