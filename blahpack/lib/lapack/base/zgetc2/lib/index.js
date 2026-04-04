
'use strict';

/**
* Computes an LU factorization with complete pivoting of a general N-by-N complex matrix.
*
* @module @stdlib/lapack/base/zgetc2
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zgetc2 = require( './main.js' );
*
* var A = new Complex128Array( [ 1.0, 2.0, 5.0, 6.0, 3.0, 4.0, 7.0, 8.0 ] );
* var IPIV = new Int32Array( 2 );
* var JPIV = new Int32Array( 2 );
*
* var info = zgetc2.ndarray( 2, A, 1, 2, 0, IPIV, 1, 0, JPIV, 1, 0 );
* // returns 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgetc2;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgetc2 = main;
} else {
	zgetc2 = tmp;
}


// EXPORTS //

module.exports = zgetc2;

// exports: { "ndarray": "zgetc2.ndarray" }
