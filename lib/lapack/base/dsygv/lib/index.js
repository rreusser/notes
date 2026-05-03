
'use strict';

/**
* Computes eigenvalues and optionally eigenvectors of a generalized symmetric-definite eigenproblem.
*
* @module @stdlib/lapack/base/dsygv
*
*
* @example
* var dsygv = require( '@stdlib/lapack/base/dsygv' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var B = discreteUniform( N * N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dsygv.ndarray( N, 'compute-vectors', 'upper', N, A, N, 1, 0, B, N, 1, 0, w, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsygv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsygv = main;
} else {
	dsygv = tmp;
}


// EXPORTS //

module.exports = dsygv;

// exports: { "ndarray": "dsygv.ndarray" }
