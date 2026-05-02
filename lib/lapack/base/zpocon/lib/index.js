

'use strict';

/**
* Estimate the reciprocal condition number of a complex positive definite matrix
*
* @module @stdlib/lapack/base/zpocon
*
*
* @example
* var zpocon = require( '@stdlib/lapack/base/zpocon' );
*
* var N = 3;
* var A = discreteUniform( N * N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
*
* zpocon.ndarray( 'upper', N, A, N, 1, 0, 1.0, 1.0, WORK, 1, 0, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zpocon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zpocon = main;
} else {
	zpocon = tmp;
}


// EXPORTS //

module.exports = zpocon;

// exports: { "ndarray": "zpocon.ndarray" }
