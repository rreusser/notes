
'use strict';

/**
* Copy a triangular matrix from Rectangular Full Packed format to standard full format.
*
* @module @stdlib/lapack/base/dtfttr
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

var dtfttr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtfttr = main;
} else {
	dtfttr = tmp;
}


// EXPORTS //

module.exports = dtfttr;

// exports: { "ndarray": "dtfttr.ndarray" }
