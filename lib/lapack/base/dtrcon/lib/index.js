
'use strict';

/**
* Estimates the reciprocal condition number of a triangular matrix.
*
* @module @stdlib/lapack/base/dtrcon
*
*
* @example
* var dtrcon = require( '@stdlib/lapack/base/dtrcon' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dtrcon.ndarray( '1', 'upper', 'non-unit', N, A, N, 1, 0, 1.0, WORK, 1, 0, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtrcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrcon = main;
} else {
	dtrcon = tmp;
}


// EXPORTS //

module.exports = dtrcon;

// exports: { "ndarray": "dtrcon.ndarray" }
