

'use strict';

/**
* Solves a complex triangular system with scaling to prevent overflow
*
* @module @stdlib/lapack/base/zlatrs
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

var zlatrs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlatrs = main;
} else {
	zlatrs = tmp;
}


// EXPORTS //

module.exports = zlatrs;

// exports: { "ndarray": "zlatrs.ndarray" }
