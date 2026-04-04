
'use strict';

/**
* Compute the reciprocal of the condition number of a real symmetric positive definite tridiagonal matrix.
*
* @module @stdlib/lapack/base/dptcon
*
* @example
* // TODO: Add example
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
