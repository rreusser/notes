
'use strict';

/**
* Reduces a symmetric definite generalized eigenproblem to standard form (unblocked).
*
* @module @stdlib/lapack/base/dsygs2
*
*
* @example
* var dsygs2 = require( '@stdlib/lapack/base/dsygs2' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
*
* dsygs2.ndarray( N, 'upper', N, A, N, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsygs2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsygs2 = main;
} else {
	dsygs2 = tmp;
}


// EXPORTS //

module.exports = dsygs2;

// exports: { "ndarray": "dsygs2.ndarray" }
