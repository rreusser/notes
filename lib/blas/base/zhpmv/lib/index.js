
'use strict';

/**
* Perform the Hermitian packed matrix-vector operation y := alpha_A_x + beta*y.
*
* @module @stdlib/blas/base/zhpmv
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

var zhpmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhpmv = main;
} else {
	zhpmv = tmp;
}


// EXPORTS //

module.exports = zhpmv;

// exports: { "ndarray": "zhpmv.ndarray" }
