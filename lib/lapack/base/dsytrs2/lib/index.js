
'use strict';

/**
* Solves a symmetric indefinite system using Bunch-Kaufman factorization with BLAS-3.
*
* @module @stdlib/lapack/base/dsytrs2
*
*
* @example
* var dsytrs2 = require( '@stdlib/lapack/base/dsytrs2' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dsytrs2.ndarray( 'upper', N, N, A, N, 1, 0, IPIV, 1, 0, B, N, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsytrs2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsytrs2 = main;
} else {
	dsytrs2 = tmp;
}


// EXPORTS //

module.exports = dsytrs2;

// exports: { "ndarray": "dsytrs2.ndarray" }
