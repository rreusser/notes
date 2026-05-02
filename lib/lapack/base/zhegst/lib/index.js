

'use strict';

/**
* Reduces a Hermitian-definite generalized eigenproblem to standard form (blocked)
*
* @module @stdlib/lapack/base/zhegst
*
*
* @example
* var zhegst = require( '@stdlib/lapack/base/zhegst' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
*
* zhegst.ndarray( N, 'upper', N, A, N, 1, 0, B, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhegst;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhegst = main;
} else {
	zhegst = tmp;
}


// EXPORTS //

module.exports = zhegst;

// exports: { "ndarray": "zhegst.ndarray" }
