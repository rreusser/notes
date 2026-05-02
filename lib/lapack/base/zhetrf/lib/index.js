

'use strict';

/**
* Complex Hermitian indefinite factorization (blocked Bunch-Kaufman)
*
* @module @stdlib/lapack/base/zhetrf
*
*
* @example
* var zhetrf = require( '@stdlib/lapack/base/zhetrf' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* zhetrf.ndarray( 'upper', N, A, N, 1, 0, IPIV, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhetrf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhetrf = main;
} else {
	zhetrf = tmp;
}


// EXPORTS //

module.exports = zhetrf;

// exports: { "ndarray": "zhetrf.ndarray" }
