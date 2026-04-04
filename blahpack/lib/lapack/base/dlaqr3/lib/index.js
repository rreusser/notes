
'use strict';

/**
* Performs aggressive early deflation with blocked operations.
*
* @module @stdlib/lapack/base/dlaqr3
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

var dlaqr3;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqr3 = main;
} else {
	dlaqr3 = tmp;
}


// EXPORTS //

module.exports = dlaqr3;

// exports: { "ndarray": "dlaqr3.ndarray" }
