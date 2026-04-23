
'use strict';

/**
* Computes the norm of a real triangular matrix.
*
* @module @stdlib/lapack/base/dlantr
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

var dlantr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlantr = main;
} else {
	dlantr = tmp;
}


// EXPORTS //

module.exports = dlantr;

// exports: { "ndarray": "dlantr.ndarray" }
