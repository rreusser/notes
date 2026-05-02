
'use strict';

/**
* Compute selected eigenvalues and optionally eigenvectors of a real symmetric matrix.
*
* @module @stdlib/lapack/base/dsyevr
*
*
* @example
* var dsyevr = require( '@stdlib/lapack/base/dsyevr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var ISUPPZ = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dsyevr.ndarray( 'compute-vectors', 'all', 'upper', N, A, N, 1, 0, 1.0, 1.0, N, N, 1.0, 1, w, 1, 0, Z, N, 1, 0, ISUPPZ, 1, 0, WORK, 1, 0, N, IWORK, 1, 0, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dsyevr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsyevr = main;
} else {
	dsyevr = tmp;
}


// EXPORTS //

module.exports = dsyevr;

// exports: { "ndarray": "dsyevr.ndarray" }
