
'use strict';

/**
* Solves a real symmetric indefinite system of linear equations using Bunch-Kaufman factorization.
*
* @module @stdlib/lapack/base/dsysv
*
*
* @example
* var dsysv = require( '@stdlib/lapack/base/dsysv' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dsysv.ndarray( 'upper', N, N, A, N, 1, 0, IPIV, 1, 0, B, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsysv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsysv = main;
} else {
	dsysv = tmp;
}


// EXPORTS //

module.exports = dsysv;

// exports: { "ndarray": "dsysv.ndarray" }
