
'use strict';

/**
* Computes a generalized QR factorization of an N-by-M matrix A and an N-by-P matrix B.
*
* @module @stdlib/lapack/base/zggqrf
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zggqrf = require( '@stdlib/lapack/base/zggqrf' );
*
* var A = new Complex128Array( [ 5, 2 ] );
* var TAUA = new Complex128Array( 1 );
* var B = new Complex128Array( [ 3, -1 ] );
* var TAUB = new Complex128Array( 1 );
* var WORK = new Complex128Array( 64 );
*
* var info = zggqrf.ndarray( 1, 1, 1, A, 1, 1, 0, TAUA, 1, 0, B, 1, 1, 0, TAUB, 1, 0, WORK, 1, 0, 64 );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zggqrf;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zggqrf = main;
} else {
	zggqrf = tmp;
}


// EXPORTS //

module.exports = zggqrf;

// exports: { "ndarray": "zggqrf.ndarray" }
