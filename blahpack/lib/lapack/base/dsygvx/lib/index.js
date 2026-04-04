
'use strict';

/**
* Computes selected eigenvalues and eigenvectors of a real generalized symmetric-definite eigenproblem.
*
* @module @stdlib/lapack/base/dsygvx
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

var dsygvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsygvx = main;
} else {
	dsygvx = tmp;
}


// EXPORTS //

module.exports = dsygvx;

// exports: { "ndarray": "dsygvx.ndarray" }
