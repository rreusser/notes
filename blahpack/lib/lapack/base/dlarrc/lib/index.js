
'use strict';

/**
* Count eigenvalues of a symmetric tridiagonal matrix in an interval.
*
* @module @stdlib/lapack/base/dlarrc
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

var dlarrc;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlarrc = main;
} else {
	dlarrc = tmp;
}


// EXPORTS //

module.exports = dlarrc;

// exports: { "ndarray": "dlarrc.ndarray" }
