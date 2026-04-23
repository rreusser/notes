

'use strict';

/**
* Compute the reciprocal of the condition number of a complex Hermitian positive definite tridiagonal matrix
*
* @module @stdlib/lapack/base/zptcon
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
