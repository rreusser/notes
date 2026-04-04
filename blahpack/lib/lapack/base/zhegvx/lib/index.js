

'use strict';

/**
* Computes selected eigenvalues and eigenvectors of a complex generalized Hermitian-definite eigenproblem
*
* @module @stdlib/lapack/base/zhegvx
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

var zhegvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhegvx = main;
} else {
	zhegvx = tmp;
}


// EXPORTS //

module.exports = zhegvx;

// exports: { "ndarray": "zhegvx.ndarray" }
