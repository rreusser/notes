
'use strict';

/**
* Compute the dot product of two vectors with extended precision accumulation.
*
* @module @stdlib/blas/base/dsdot
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

var dsdot;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dsdot = main;
} else {
	dsdot = tmp;
}


// EXPORTS //

module.exports = dsdot;

// exports: { "ndarray": "dsdot.ndarray" }
