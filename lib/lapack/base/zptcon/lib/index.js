

'use strict';

/**
* Compute the reciprocal of the condition number of a complex Hermitian positive definite tridiagonal matrix
*
* @module @stdlib/lapack/base/zptcon
*
*
* @example
* var zptcon = require( '@stdlib/lapack/base/zptcon' );
*
* var N = 3;
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
* var RWORK = discreteUniform( N, -10, 10, opts );
*
* zptcon.ndarray( N, d, 1, 0, e, 1, 0, 1.0, 1.0, RWORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zptcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zptcon = main;
} else {
	zptcon = tmp;
}


// EXPORTS //

module.exports = zptcon;

// exports: { "ndarray": "zptcon.ndarray" }
