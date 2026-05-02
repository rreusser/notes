

'use strict';

/**
* Solve a system of linear equations A*X = B with a Hermitian indefinite matrix using Bunch-Kaufman factorization
*
* @module @stdlib/lapack/base/zhetrs
*
*
* @example
* var zhetrs = require( '@stdlib/lapack/base/zhetrs' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
*
* zhetrs.ndarray( 'upper', N, N, A, N, 1, 0, IPIV, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhetrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhetrs = main;
} else {
	zhetrs = tmp;
}


// EXPORTS //

module.exports = zhetrs;

// exports: { "ndarray": "zhetrs.ndarray" }
