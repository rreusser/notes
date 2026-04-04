
'use strict';

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a symmetric positive-definite matrix.
*
* @module @stdlib/lapack/base/dla_porpvgrw
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

var dla_porpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dla_porpvgrw = main;
} else {
	dla_porpvgrw = tmp;
}


// EXPORTS //

module.exports = dla_porpvgrw;

// exports: { "ndarray": "dla_porpvgrw.ndarray" }
