

'use strict';

/**
* Copies a complex triangular matrix from packed storage to full storage
*
* @module @stdlib/lapack/base/ztpttr
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

var ztpttr;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztpttr = main;
} else {
	ztpttr = tmp;
}


// EXPORTS //

module.exports = ztpttr;

// exports: { "ndarray": "ztpttr.ndarray" }
