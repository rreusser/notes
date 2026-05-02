
'use strict';

/**
* Computes a generalized QR factorization of matrices A and B.
*
* @module @stdlib/lapack/base/dggqrf
*
*
* @example
* var dggqrf = require( '@stdlib/lapack/base/dggqrf' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var TAUA = discreteUniform( N, -10, 10, opts );
* var TAUB = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dggqrf.ndarray( N, N, 1, A, N, 1, 0, TAUA, 1, 0, B, N, 1, 0, TAUB, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dggqrf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dggqrf = main;
} else {
	dggqrf = tmp;
}


// EXPORTS //

module.exports = dggqrf;

// exports: { "ndarray": "dggqrf.ndarray" }
