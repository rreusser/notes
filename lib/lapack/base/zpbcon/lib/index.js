

'use strict';

/**
* Estimate reciprocal condition number of complex positive definite band matrix
*
* @module @stdlib/lapack/base/zpbcon
*
*
* @example
* var zpbcon = require( '@stdlib/lapack/base/zpbcon' );
*
* var N = 3;
* var AB = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
*
* zpbcon.ndarray( 'upper', N, 1, AB, N, 1, 0, 1.0, 1.0, WORK, 1, 0, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zpbcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpbcon = main;
} else {
	zpbcon = tmp;
}


// EXPORTS //

module.exports = zpbcon;

// exports: { "ndarray": "zpbcon.ndarray" }
