
'use strict';

/**
* Compute the reciprocal of the condition number of a real symmetric positive definite tridiagonal matrix.
*
* @module @stdlib/lapack/base/dptcon
*
*
* @example
* var dptcon = require( '@stdlib/lapack/base/dptcon' );
*
* var N = 3;
* var d = discreteUniform( N, -10, 10, opts );
* var e = discreteUniform( N, -10, 10, opts );
* var WORK = discreteUniform( N, -10, 10, opts );
*
* dptcon.ndarray( N, d, 1, 0, e, 1, 0, 1.0, 1.0, WORK, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dptcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dptcon = main;
} else {
	dptcon = tmp;
}


// EXPORTS //

module.exports = dptcon;

// exports: { "ndarray": "dptcon.ndarray" }
