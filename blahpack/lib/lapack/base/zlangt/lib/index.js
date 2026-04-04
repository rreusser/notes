'use strict';

/**
* Returns the norm of a complex tridiagonal matrix.
*
* @module @stdlib/lapack/base/zlangt
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zlangt = require( '@stdlib/lapack/base/zlangt' );
*
* var dl = new Complex128Array( [ 3.0, 2.0, 1.0, 4.0, 2.0, 1.0 ] );
* var d = new Complex128Array( [ 2.0, 1.0, 4.0, 2.0, 5.0, 3.0, 6.0, 1.0 ] );
* var du = new Complex128Array( [ -1.0, 3.0, -2.0, 1.0, -3.0, 2.0 ] );
*
* var result = zlangt.ndarray( 'max', 4, dl, 1, 0, d, 1, 0, du, 1, 0 );
* // returns ~6.083
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlangt;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlangt = main;
} else {
	zlangt = tmp;
}


// EXPORTS //

module.exports = zlangt;

// exports: { "ndarray": "zlangt.ndarray" }
