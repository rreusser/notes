
'use strict';

/**
* Perform matrix-vector operation with a general band matrix.
*
* @module @stdlib/blas/base/dgbmv
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

var dgbmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgbmv = main;
} else {
	dgbmv = tmp;
}


// EXPORTS //

module.exports = dgbmv;

// exports: { "ndarray": "dgbmv.ndarray" }
