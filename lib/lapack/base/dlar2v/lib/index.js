
'use strict';

/**
* Apply a vector of real plane rotations from both sides to a sequence of 2-by-2 symmetric matrices.
*
* @module @stdlib/lapack/base/dlar2v
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

var dlar2v;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dlar2v = main;
} else {
	dlar2v = tmp;
}


// EXPORTS //

module.exports = dlar2v;

// exports: { "ndarray": "dlar2v.ndarray" }
