
'use strict';

/**
* Reorders the Schur factorization and computes condition numbers.
*
* @module @stdlib/lapack/base/dtrsen
*
*
* @example
* var dtrsen = require( '@stdlib/lapack/base/dtrsen' );
*
* var N = 3;
* var T = discreteUniform( N * N, -10, 10, opts );
* var Q = discreteUniform( N * N, -10, 10, opts );
* var SELECT = discreteUniform( N, -10, 10, opts );
* var WR = discreteUniform( N, -10, 10, opts );
* var WI = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dtrsen.ndarray( 'both', 'update', SELECT, 1, 0, N, T, N, 1, 0, Q, N, 1, 0, WR, 1, 0, WI, 1, 0, N, 1.0, 1, WORK, 1, 0, N, IWORK, 1, 0, 1 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtrsen;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrsen = main;
} else {
	dtrsen = tmp;
}


// EXPORTS //

module.exports = dtrsen;

// exports: { "ndarray": "dtrsen.ndarray" }
