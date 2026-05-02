

'use strict';

/**
* Reduces a Hermitian-definite generalized eigenproblem to standard form (unblocked)
*
* @module @stdlib/lapack/base/zhegs2
*
*
* @example
* var zhegs2 = require( '@stdlib/lapack/base/zhegs2' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
*
* zhegs2.ndarray( N, 'upper', N, A, N, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhegs2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhegs2 = main;
} else {
	zhegs2 = tmp;
}


// EXPORTS //

module.exports = zhegs2;

// exports: { "ndarray": "zhegs2.ndarray" }
