
'use strict';

/**
* Computes a QR factorization with column pivoting using Level 2 BLAS.
*
* @module @stdlib/lapack/base/dlaqp2
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

var dlaqp2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqp2 = main;
} else {
	dlaqp2 = tmp;
}


// EXPORTS //

module.exports = dlaqp2;

// exports: { "ndarray": "dlaqp2.ndarray" }
