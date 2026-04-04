
'use strict';

/**
* Compute selected eigenvalues and optionally eigenvectors of a real symmetric matrix.
*
* @module @stdlib/lapack/base/dsyevr
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

var dsyevr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsyevr = main;
} else {
	dsyevr = tmp;
}


// EXPORTS //

module.exports = dsyevr;

// exports: { "ndarray": "dsyevr.ndarray" }
