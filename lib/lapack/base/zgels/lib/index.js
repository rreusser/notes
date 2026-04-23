
'use strict';

/**
* Solve complex linear least squares using QR or LQ factorization.
*
* @module @stdlib/lapack/base/zgels
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

var zgels;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgels = main;
} else {
	zgels = tmp;
}


// EXPORTS //

module.exports = zgels;

// exports: { "ndarray": "zgels.ndarray" }
