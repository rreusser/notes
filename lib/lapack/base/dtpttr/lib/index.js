
'use strict';

/**
* Copy a triangular matrix from standard packed format to full format.
*
* @module @stdlib/lapack/base/dtpttr
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

var dtpttr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtpttr = main;
} else {
	dtpttr = tmp;
}


// EXPORTS //

module.exports = dtpttr;

// exports: { "ndarray": "dtpttr.ndarray" }
