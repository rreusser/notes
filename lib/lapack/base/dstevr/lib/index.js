
'use strict';

/**
* Compute selected eigenvalues and optionally eigenvectors of a real symmetric tridiagonal matrix.
*
* @module @stdlib/lapack/base/dstevr
*
*
* @example
* var dstevr = require( '@stdlib/lapack/base/dstevr' );
*
* var N = 3;
* var Z = discreteUniform( N * N, -10, 10, opts );
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var ISUPPZ = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dstevr.ndarray( 'compute-vectors', 'all', N, d, 1, 0, e, 1, 0, 1.0, 1.0, N, N, 1.0, 1, w, 1, 0, Z, N, 1, 0, ISUPPZ, 1, 0, WORK, 1, 0, N, IWORK, 1, 0, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dstevr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dstevr = main;
} else {
	dstevr = tmp;
}


// EXPORTS //

module.exports = dstevr;

// exports: { "ndarray": "dstevr.ndarray" }
