
'use strict';

/**
* Compute selected eigenvalues and optionally eigenvectors of a real symmetric matrix.
*
* @module @stdlib/lapack/base/dsyevx
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

var dsyevx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsyevx = main;
} else {
	dsyevx = tmp;
}


// EXPORTS //

module.exports = dsyevx;

// exports: { "ndarray": "dsyevx.ndarray" }
