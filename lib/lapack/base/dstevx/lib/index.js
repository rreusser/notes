
'use strict';

/**
* Computes selected eigenvalues and eigenvectors of a real symmetric tridiagonal matrix.
*
* @module @stdlib/lapack/base/dstevx
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

var dstevx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dstevx = main;
} else {
	dstevx = tmp;
}


// EXPORTS //

module.exports = dstevx;

// exports: { "ndarray": "dstevx.ndarray" }
