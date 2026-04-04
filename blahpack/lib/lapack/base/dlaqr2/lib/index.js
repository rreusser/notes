
'use strict';

/**
* Performs aggressive early deflation on an upper Hessenberg matrix.
*
* @module @stdlib/lapack/base/dlaqr2
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

var dlaqr2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqr2 = main;
} else {
	dlaqr2 = tmp;
}


// EXPORTS //

module.exports = dlaqr2;

// exports: { "ndarray": "dlaqr2.ndarray" }
