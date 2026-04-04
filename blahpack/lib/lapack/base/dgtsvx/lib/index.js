
'use strict';

/**
* Expert driver for solving a general tridiagonal system.
*
* @module @stdlib/lapack/base/dgtsvx
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

var dgtsvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgtsvx = main;
} else {
	dgtsvx = tmp;
}


// EXPORTS //

module.exports = dgtsvx;

// exports: { "ndarray": "dgtsvx.ndarray" }
