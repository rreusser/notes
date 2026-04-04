
'use strict';

/**
* Perform matrix-vector operation with a triangular band matrix.
*
* @module @stdlib/blas/base/dtbmv
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

var dtbmv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtbmv = main;
} else {
	dtbmv = tmp;
}


// EXPORTS //

module.exports = dtbmv;

// exports: { "ndarray": "dtbmv.ndarray" }
