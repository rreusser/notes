

'use strict';

/**
* Computes all eigenvalues and optionally eigenvectors of a complex generalized Hermitian-definite eigenproblem
*
* @module @stdlib/lapack/base/zhegv
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

var zhegv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhegv = main;
} else {
	zhegv = tmp;
}


// EXPORTS //

module.exports = zhegv;

// exports: { "ndarray": "zhegv.ndarray" }
