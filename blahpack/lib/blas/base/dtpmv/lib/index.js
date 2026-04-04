
'use strict';

/**
* Perform matrix-vector operation with a triangular packed matrix.
*
* @module @stdlib/blas/base/dtpmv
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

var dtpmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtpmv = main;
} else {
	dtpmv = tmp;
}


// EXPORTS //

module.exports = dtpmv;

// exports: { "ndarray": "dtpmv.ndarray" }
