

'use strict';

/**
* Solves a complex symmetric indefinite system using Bunch-Kaufman factorization with BLAS-3
*
* @module @stdlib/lapack/base/zsytrs2
*
*
* @example
* var zsytrs2 = require( '@stdlib/lapack/base/zsytrs2' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zsytrs2.ndarray( 'upper', N, N, A, N, 1, 0, IPIV, 1, 0, B, N, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zsytrs2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zsytrs2 = main;
} else {
	zsytrs2 = tmp;
}


// EXPORTS //

module.exports = zsytrs2;

// exports: { "ndarray": "zsytrs2.ndarray" }
