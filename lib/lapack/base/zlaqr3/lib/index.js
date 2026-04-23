

'use strict';

/**
* Complex aggressive early deflation (recursive)
*
* @module @stdlib/lapack/base/zlaqr3
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

var zlaqr3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqr3 = main;
} else {
	zlaqr3 = tmp;
}


// EXPORTS //

module.exports = zlaqr3;

// exports: { "ndarray": "zlaqr3.ndarray" }
