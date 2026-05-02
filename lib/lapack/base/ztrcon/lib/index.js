

'use strict';

/**
* Estimate the reciprocal condition number of a complex triangular matrix
*
* @module @stdlib/lapack/base/ztrcon
*
*
* @example
* var ztrcon = require( '@stdlib/lapack/base/ztrcon' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
*
* ztrcon.ndarray( '1', 'upper', 'non-unit', N, A, N, 1, 0, 1.0, WORK, 1, 0, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var ztrcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrcon = main;
} else {
	ztrcon = tmp;
}


// EXPORTS //

module.exports = ztrcon;

// exports: { "ndarray": "ztrcon.ndarray" }
