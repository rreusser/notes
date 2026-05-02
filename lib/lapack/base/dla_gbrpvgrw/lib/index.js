
'use strict';

/**
* Compute the reciprocal pivot growth factor norm(A)/norm(U) for a general banded matrix.
*
* @module @stdlib/lapack/base/dla_gbrpvgrw
*
*
* @example
* var dla_gbrpvgrw = require( '@stdlib/lapack/base/dla_gbrpvgrw' );
*
* var N = 3;
* var AB = discreteUniform( N * N, -10, 10, opts );
* var AFB = discreteUniform( N * N, -10, 10, opts );
*
* dla_gbrpvgrw.ndarray( N, N, N, 1, AB, N, 1, 0, AFB, N, 1, 0 );
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dla_gbrpvgrw;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dla_gbrpvgrw = main;
} else {
	dla_gbrpvgrw = tmp;
}


// EXPORTS //

module.exports = dla_gbrpvgrw;

// exports: { "ndarray": "dla_gbrpvgrw.ndarray" }
