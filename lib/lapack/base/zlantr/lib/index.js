

'use strict';

/**
* Computes the norm of a complex triangular matrix
*
* @module @stdlib/lapack/base/zlantr
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

var zlantr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlantr = main;
} else {
	zlantr = tmp;
}


// EXPORTS //

module.exports = zlantr;

// exports: { "ndarray": "zlantr.ndarray" }
