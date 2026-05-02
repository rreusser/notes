
'use strict';

/**
* LU factorization with complete pivoting of a general NxN matrix.
*
* @module @stdlib/lapack/base/dgetc2
*
*
* @example
* var dgetc2 = require( '@stdlib/lapack/base/dgetc2' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var JPIV = discreteUniform( N, -10, 10, opts );
*
* dgetc2.ndarray( N, A, N, 1, 0, IPIV, 1, 0, JPIV, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgetc2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgetc2 = main;
} else {
	dgetc2 = tmp;
}


// EXPORTS //

module.exports = dgetc2;

// exports: { "ndarray": "dgetc2.ndarray" }
