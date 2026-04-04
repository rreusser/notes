
'use strict';

/**
* Copy a triangular matrix from standard packed format (TP) to Rectangular Full Packed format (RFP).
*
* @module @stdlib/lapack/base/dtpttf
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

var dtpttf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtpttf = main;
} else {
	dtpttf = tmp;
}


// EXPORTS //

module.exports = dtpttf;

// exports: { "ndarray": "dtpttf.ndarray" }
