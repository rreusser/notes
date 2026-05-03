
'use strict';

/**
* Estimate reciprocal condition number of complex general band matrix.
*
* @module @stdlib/lapack/base/zgbcon
*
*
* @example
* var zgbcon = require( '@stdlib/lapack/base/zgbcon' );
*
* var N = 3;
* var AB = discreteUniform( N * N, -10, 10, opts );
* var IPIV = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
*
* zgbcon.ndarray( 'one-norm', N, N, N, AB, N, 1, 0, IPIV, 1, 0, 1.0, 1.0, WORK, 1, 0, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgbcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgbcon = main;
} else {
	zgbcon = tmp;
}


// EXPORTS //

module.exports = zgbcon;

// exports: { "ndarray": "zgbcon.ndarray" }
