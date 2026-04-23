
'use strict';

/**
* Applies an elementary reflector to a general matrix with unrolled loops.
*
* @module @stdlib/lapack/base/dlarfx
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

var dlarfx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlarfx = main;
} else {
	dlarfx = tmp;
}


// EXPORTS //

module.exports = dlarfx;

// exports: { "ndarray": "dlarfx.ndarray" }
