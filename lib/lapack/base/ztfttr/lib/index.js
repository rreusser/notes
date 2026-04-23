

'use strict';

/**
* Copy a triangular matrix from Rectangular Full Packed format (RFP) to standard full format (TR)
*
* @module @stdlib/lapack/base/ztfttr
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

var ztfttr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztfttr = main;
} else {
	ztfttr = tmp;
}


// EXPORTS //

module.exports = ztfttr;

// exports: { "ndarray": "ztfttr.ndarray" }
