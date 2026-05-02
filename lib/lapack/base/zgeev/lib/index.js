
'use strict';

/**
* Computes eigenvalues and eigenvectors of a complex general matrix.
*
* @module @stdlib/lapack/base/zgeev
*
*
* @example
* var zgeev = require( '@stdlib/lapack/base/zgeev' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var VL = discreteUniform( N * N, -10, 10, opts );
* var VR = discreteUniform( N * N, -10, 10, opts );
* var w = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
*
* zgeev.ndarray( 'compute-vectors', 'compute-vectors', N, A, N, 1, 0, w, 1, 0, VL, N, 1, 0, VR, N, 1, 0, WORK, 1, 0, N, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgeev;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgeev = main;
} else {
	zgeev = tmp;
}


// EXPORTS //

module.exports = zgeev;

// exports: { "ndarray": "zgeev.ndarray" }
