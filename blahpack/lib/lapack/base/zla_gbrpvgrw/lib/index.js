

'use strict';

/**
* Compute the reciprocal pivot growth factor for a complex general banded matrix.
*
* @module @stdlib/lapack/base/zla_gbrpvgrw
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

var zla_gbrpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zla_gbrpvgrw = main;
} else {
	zla_gbrpvgrw = tmp;
}


// EXPORTS //

module.exports = zla_gbrpvgrw;

// exports: { "ndarray": "zla_gbrpvgrw.ndarray" }
