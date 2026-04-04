

'use strict';

/**
* Complex multishift QR with aggressive early deflation (non-recursive)
*
* @module @stdlib/lapack/base/zlaqr4
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

var zlaqr4;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlaqr4 = main;
} else {
	zlaqr4 = tmp;
}


// EXPORTS //

module.exports = zlaqr4;

// exports: { "ndarray": "zlaqr4.ndarray" }
