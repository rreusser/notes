
'use strict';

/**
* Compute eigenvalues and Schur decomposition of a complex matrix.
*
* @module @stdlib/lapack/base/zgees
*
*
* @example
* var zgees = require( '@stdlib/lapack/base/zgees' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var VS = discreteUniform( N * N, -10, 10, opts );
* var W = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
* var BWORK = discreteUniform( N, -10, 10, opts );
*
* zgees.ndarray( 'compute-vectors', 'none', 1, N, A, N, 1, 0, 1, W, 1, 0, VS, N, 1, 0, WORK, 1, 0, N, RWORK, 1, 0, BWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgees;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgees = main;
} else {
	zgees = tmp;
}


// EXPORTS //

module.exports = zgees;

// exports: { "ndarray": "zgees.ndarray" }
