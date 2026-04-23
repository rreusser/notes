
'use strict';

/**
* Perform the Hermitian banded matrix-vector operation y := alpha_A_x + beta*y.
*
* @module @stdlib/blas/base/zhbmv
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

var zhbmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhbmv = main;
} else {
	zhbmv = tmp;
}


// EXPORTS //

module.exports = zhbmv;

// exports: { "ndarray": "zhbmv.ndarray" }
