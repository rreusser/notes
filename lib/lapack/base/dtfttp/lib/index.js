
'use strict';

/**
* Copy a triangular matrix from Rectangular Full Packed (RFP) format to standard packed format (TP).
*
* @module @stdlib/lapack/base/dtfttp
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

var dtfttp;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtfttp = main;
} else {
	dtfttp = tmp;
}


// EXPORTS //

module.exports = dtfttp;

// exports: { "ndarray": "dtfttp.ndarray" }
