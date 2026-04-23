
'use strict';

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a symmetric indefinite matrix.
*
* @module @stdlib/lapack/base/dla_syrpvgrw
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

var dla_syrpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dla_syrpvgrw = main;
} else {
	dla_syrpvgrw = tmp;
}


// EXPORTS //

module.exports = dla_syrpvgrw;

// exports: { "ndarray": "dla_syrpvgrw.ndarray" }
