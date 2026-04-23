
'use strict';

/**
* Compute selected eigenvalues and optionally eigenvectors of a real symmetric tridiagonal matrix.
*
* @module @stdlib/lapack/base/dstevr
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

var dstevr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dstevr = main;
} else {
	dstevr = tmp;
}


// EXPORTS //

module.exports = dstevr;

// exports: { "ndarray": "dstevr.ndarray" }
