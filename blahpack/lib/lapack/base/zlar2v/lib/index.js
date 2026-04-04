

'use strict';

/**
* Apply a vector of complex plane rotations with real cosines from both sides to a sequence of 2-by-2 complex Hermitian matrices.
*
* @module @stdlib/lapack/base/zlar2v
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

var zlar2v;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlar2v = main;
} else {
	zlar2v = tmp;
}


// EXPORTS //

module.exports = zlar2v;

// exports: { "ndarray": "zlar2v.ndarray" }
