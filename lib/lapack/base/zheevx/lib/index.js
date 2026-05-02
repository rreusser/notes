

'use strict';

/**
* Computes selected eigenvalues and eigenvectors of a complex Hermitian matrix
*
* @module @stdlib/lapack/base/zheevx
*
*
* @example
* var zheevx = require( '@stdlib/lapack/base/zheevx' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
* var IFAIL = discreteUniform( N, -10, 10, opts );
*
* zheevx.ndarray( 'compute-vectors', 'all', 'upper', N, A, N, 1, 0, 1.0, 1.0, N, N, 1.0, 1, w, 1, 0, Z, N, 1, 0, WORK, 1, 0, N, RWORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zheevx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zheevx = main;
} else {
	zheevx = tmp;
}


// EXPORTS //

module.exports = zheevx;

// exports: { "ndarray": "zheevx.ndarray" }
