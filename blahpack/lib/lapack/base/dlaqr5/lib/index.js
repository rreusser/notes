
'use strict';

/**
* Performs a single small-bulge multi-shift QR sweep.
*
* @module @stdlib/lapack/base/dlaqr5
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

var dlaqr5;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqr5 = main;
} else {
	dlaqr5 = tmp;
}


// EXPORTS //

module.exports = dlaqr5;

// exports: { "ndarray": "dlaqr5.ndarray" }
