
'use strict';

/**
* Estimate the reciprocal condition number of a complex general matrix.
*
* @module @stdlib/lapack/base/zgecon
*
*
* @example
* var zgecon = require( '@stdlib/lapack/base/zgecon' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
*
* zgecon.ndarray( '1', N, A, N, 1, 0, 1.0, 1.0, WORK, 1, 0, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgecon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgecon = main;
} else {
	zgecon = tmp;
}


// EXPORTS //

module.exports = zgecon;

// exports: { "ndarray": "zgecon.ndarray" }
