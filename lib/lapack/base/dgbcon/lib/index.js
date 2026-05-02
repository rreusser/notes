
'use strict';

/**
* Estimates the reciprocal condition number of a general banded matrix.
*
* @module @stdlib/lapack/base/dgbcon
*
*
* @example
* var dgbcon = require( '@stdlib/lapack/base/dgbcon' );
*
* var N = 3;
* var AB = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var IWORK = discreteUniform( N, -10, 10, opts );
*
* dgbcon.ndarray( '1', N, N, N, AB, N, 1, 0, IPIV, 1, 0, 1.0, 1.0, WORK, 1, 0, IWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dgbcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgbcon = main;
} else {
	dgbcon = tmp;
}


// EXPORTS //

module.exports = dgbcon;

// exports: { "ndarray": "dgbcon.ndarray" }
