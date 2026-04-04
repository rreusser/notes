

'use strict';

/**
* Copy a complex triangular matrix from full format (TR) to standard packed format (TP)
*
* @module @stdlib/lapack/base/ztrttp
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

var ztrttp;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	ztrttp = main;
} else {
	ztrttp = tmp;
}


// EXPORTS //

module.exports = ztrttp;

// exports: { "ndarray": "ztrttp.ndarray" }
