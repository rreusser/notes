
'use strict';

/**
* Compute the norm of a general tridiagonal matrix.
*
* @module @stdlib/lapack/base/dlangt
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

var dlangt;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlangt = main;
} else {
	dlangt = tmp;
}


// EXPORTS //

module.exports = dlangt;

// exports: { "ndarray": "dlangt.ndarray" }
