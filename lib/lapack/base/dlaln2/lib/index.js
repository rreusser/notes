
'use strict';

/**
* Solves a 1x1 or 2x2 linear system with scaling to prevent overflow.
*
* @module @stdlib/lapack/base/dlaln2
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

var dlaln2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlaln2 = main;
} else {
	dlaln2 = tmp;
}


// EXPORTS //

module.exports = dlaln2;

// exports: { "ndarray": "dlaln2.ndarray" }
