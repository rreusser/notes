
'use strict';

/**
* Computes selected eigenvalues and eigenvectors of a real symmetric tridiagonal matrix.
*
* @module @stdlib/lapack/base/dstevx
*
*
* @example
* var dstevx = require( '@stdlib/lapack/base/dstevx' );
*
* var N = 3;
* var Z = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
* var IFAIL = discreteUniform( N, -10, 10, opts );
*
* dstevx.ndarray( 'compute-vectors', 'all', N, d, 1, 0, e, 1, 0, 1.0, 1.0, N, N, 1.0, N, w, 1, 0, Z, N, 1, 0, WORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dstevx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dstevx = main;
} else {
	dstevx = tmp;
}


// EXPORTS //

module.exports = dstevx;

// exports: { "ndarray": "dstevx.ndarray" }
