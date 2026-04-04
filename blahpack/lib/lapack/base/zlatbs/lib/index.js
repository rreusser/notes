

'use strict';

/**
* Complex triangular banded solve with scaling
*
* @module @stdlib/lapack/base/zlatbs
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

var zlatbs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlatbs = main;
} else {
	zlatbs = tmp;
}


// EXPORTS //

module.exports = zlatbs;

// exports: { "ndarray": "zlatbs.ndarray" }
