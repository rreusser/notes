

'use strict';

/**
* Complex Hermitian indefinite panel factorization (blocked Bunch-Kaufman)
*
* @module @stdlib/lapack/base/zlahef
*
*
* @example
* var zlahef = require( '@stdlib/lapack/base/zlahef' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var W = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
*
* zlahef.ndarray( 'upper', N, N, N, A, N, 1, 0, IPIV, 1, 0, W, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlahef;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlahef = main;
} else {
	zlahef = tmp;
}


// EXPORTS //

module.exports = zlahef;

// exports: { "ndarray": "zlahef.ndarray" }
