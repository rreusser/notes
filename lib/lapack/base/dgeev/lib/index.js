

'use strict';

/**
* Computes eigenvalues and eigenvectors of a real general matrix
*
* @module @stdlib/lapack/base/dgeev
*
*
* @example
* var dgeev = require( '@stdlib/lapack/base/dgeev' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var VL = discreteUniform( N * N, -10, 10, opts );
* var VR = discreteUniform( N * N, -10, 10, opts );
* var WR = discreteUniform( N, -10, 10, opts );
* var WI = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dgeev.ndarray( 'compute-vectors', 'compute-vectors', N, A, N, 1, 0, WR, 1, 0, WI, 1, 0, VL, N, 1, 0, VR, N, 1, 0, WORK, 1, 0, N );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgeev;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgeev = main;
} else {
	dgeev = tmp;
}


// EXPORTS //

module.exports = dgeev;

// exports: { "ndarray": "dgeev.ndarray" }
