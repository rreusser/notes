
'use strict';

/**
* Computes selected eigenvalues and eigenvectors of a complex Hermitian matrix using MRRR.
*
* @module @stdlib/lapack/base/zheevr
*
*
* @example
* var zheevr = require( '@stdlib/lapack/base/zheevr' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var Z = discreteUniform( N * N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var ISUPPZ = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* zheevr.ndarray( 'compute-vectors', 'all', 'upper', N, A, N, 1, 0, 1.0, 1.0, N, N, 1.0, 1, w, 1, 0, Z, N, 1, 0, ISUPPZ, 1, 0, WORK, 1, 0, N, RWORK, 1, 0, 1, IWORK, 1, 0, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zheevr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zheevr = main;
} else {
	zheevr = tmp;
}


// EXPORTS //

module.exports = zheevr;

// exports: { "ndarray": "zheevr.ndarray" }
