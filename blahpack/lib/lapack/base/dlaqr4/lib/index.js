
'use strict';

/**
* Multi-shift QR algorithm for eigenvalues of a Hessenberg matrix.
*
* @module @stdlib/lapack/base/dlaqr4
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

var dlaqr4;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqr4 = main;
} else {
	dlaqr4 = tmp;
}


// EXPORTS //

module.exports = dlaqr4;

// exports: { "ndarray": "dlaqr4.ndarray" }
