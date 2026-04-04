
'use strict';

/**
* Expert driver for symmetric positive definite solve with equilibration, condition estimation, and refinement.
*
* @module @stdlib/lapack/base/dposvx
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

var dposvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dposvx = main;
} else {
	dposvx = tmp;
}


// EXPORTS //

module.exports = dposvx;

// exports: { "ndarray": "dposvx.ndarray" }
