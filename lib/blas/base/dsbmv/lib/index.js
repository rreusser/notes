
'use strict';

/**
* Perform matrix-vector operation with a symmetric band matrix.
*
* @module @stdlib/blas/base/dsbmv
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

var dsbmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsbmv = main;
} else {
	dsbmv = tmp;
}


// EXPORTS //

module.exports = dsbmv;

// exports: { "ndarray": "dsbmv.ndarray" }
