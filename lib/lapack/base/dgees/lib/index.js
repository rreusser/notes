
'use strict';

/**
* Computes eigenvalues and Schur decomposition of a real general matrix.
*
* @module @stdlib/lapack/base/dgees
*
*
* @example
* var dgees = require( '@stdlib/lapack/base/dgees' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var VS = discreteUniform( N * N, -10, 10, opts );
* var WR = discreteUniform( N, -10, 10, opts );
* var WI = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var BWORK = discreteUniform( N, -10, 10, opts );
*
* dgees.ndarray( 'compute-vectors', 'no-sort', 1, N, A, N, 1, 0, 1, WR, 1, 0, WI, 1, 0, VS, N, 1, 0, WORK, 1, 0, N, BWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgees;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgees = main;
} else {
	dgees = tmp;
}


// EXPORTS //

module.exports = dgees;

// exports: { "ndarray": "dgees.ndarray" }
