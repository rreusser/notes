
'use strict';

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a complex symmetric indefinite matrix.
*
* @module @stdlib/lapack/base/zla_syrpvgrw
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

var zla_syrpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zla_syrpvgrw = main;
} else {
	zla_syrpvgrw = tmp;
}


// EXPORTS //

module.exports = zla_syrpvgrw;

// exports: { "ndarray": "zla_syrpvgrw.ndarray" }
