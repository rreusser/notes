

'use strict';

/**
* Expert driver for solving a general system with equilibration and condition estimation
*
* @module @stdlib/lapack/base/dgesvx
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

var dgesvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dgesvx = main;
} else {
	dgesvx = tmp;
}


// EXPORTS //

module.exports = dgesvx;

// exports: { "ndarray": "dgesvx.ndarray" }
