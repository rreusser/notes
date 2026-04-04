
'use strict';

/**
* Computes a step of QR factorization with column pivoting using Level 3 BLAS.
*
* @module @stdlib/lapack/base/dlaqps
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

var dlaqps;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaqps = main;
} else {
	dlaqps = tmp;
}


// EXPORTS //

module.exports = dlaqps;

// exports: { "ndarray": "dlaqps.ndarray" }
