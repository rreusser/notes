

'use strict';

/**
* Complex multishift QR top-level driver
*
* @module @stdlib/lapack/base/zlaqr0
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

var zlaqr0;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqr0 = main;
} else {
	zlaqr0 = tmp;
}


// EXPORTS //

module.exports = zlaqr0;

// exports: { "ndarray": "zlaqr0.ndarray" }
