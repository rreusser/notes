

'use strict';

/**
* Complex Hermitian indefinite solve using factorization from ZHETRF
*
* @module @stdlib/lapack/base/zhetrs2
*
*
* @example
* var zhetrs2 = require( '@stdlib/lapack/base/zhetrs2' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zhetrs2.ndarray( 'upper', N, N, A, N, 1, 0, IPIV, 1, 0, B, N, 1, 0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhetrs2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhetrs2 = main;
} else {
	zhetrs2 = tmp;
}


// EXPORTS //

module.exports = zhetrs2;

// exports: { "ndarray": "zhetrs2.ndarray" }
