

'use strict';

/**
* Set initial vector for Francis QR step
*
* @module @stdlib/lapack/base/zlaqr1
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

var zlaqr1;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqr1 = main;
} else {
	zlaqr1 = tmp;
}


// EXPORTS //

module.exports = zlaqr1;

// exports: { "ndarray": "zlaqr1.ndarray" }
