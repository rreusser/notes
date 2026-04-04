

'use strict';

/**
* Complex multi-shift QR sweep
*
* @module @stdlib/lapack/base/zlaqr5
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

var zlaqr5;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqr5 = main;
} else {
	zlaqr5 = tmp;
}


// EXPORTS //

module.exports = zlaqr5;

// exports: { "ndarray": "zlaqr5.ndarray" }
