

'use strict';

/**
* Complex aggressive early deflation (non-recursive)
*
* @module @stdlib/lapack/base/zlaqr2
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

var zlaqr2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqr2 = main;
} else {
	zlaqr2 = tmp;
}


// EXPORTS //

module.exports = zlaqr2;

// exports: { "ndarray": "zlaqr2.ndarray" }
