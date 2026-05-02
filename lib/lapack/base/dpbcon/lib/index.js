
'use strict';

/**
* Estimates the reciprocal condition number of a positive definite banded matrix.
*
* @module @stdlib/lapack/base/dpbcon
*
*
* @example
* var dpbcon = require( '@stdlib/lapack/base/dpbcon' );
*
* var N = 3;
* var AB = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dpbcon.ndarray( 'upper', N, 1, AB, N, 1, 0, 1.0, 1.0, WORK, 1, 0, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dpbcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dpbcon = main;
} else {
	dpbcon = tmp;
}


// EXPORTS //

module.exports = dpbcon;

// exports: { "ndarray": "dpbcon.ndarray" }
