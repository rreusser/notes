

'use strict';

/**
* Reduces a Hermitian-definite generalized eigenproblem to standard form (blocked)
*
* @module @stdlib/lapack/base/zhegst
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

var zhegst;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhegst = main;
} else {
	zhegst = tmp;
}


// EXPORTS //

module.exports = zhegst;

// exports: { "ndarray": "zhegst.ndarray" }
