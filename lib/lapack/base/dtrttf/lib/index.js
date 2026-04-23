
'use strict';

/**
* Copy a triangular matrix from standard full format (TR) to Rectangular Full Packed format (RFP).
*
* @module @stdlib/lapack/base/dtrttf
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

var dtrttf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtrttf = main;
} else {
	dtrttf = tmp;
}


// EXPORTS //

module.exports = dtrttf;

// exports: { "ndarray": "dtrttf.ndarray" }
