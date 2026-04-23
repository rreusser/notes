'use strict';

/**
* Returns the norm of a complex Hermitian tridiagonal matrix.
*
* @module @stdlib/lapack/base/zlanht
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zlanht = require( '@stdlib/lapack/base/zlanht' );
*
* var d = new Float64Array( [ 2.0, -4.0, 6.0, -1.0 ] );
* var e = new Complex128Array( [ 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 ] );
*
* var result = zlanht.ndarray( 'max', 4, d, 1, 0, e, 1, 0 );
* // returns 40.0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zlanht;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zlanht = main;
} else {
	zlanht = tmp;
}


// EXPORTS //

module.exports = zlanht;

// exports: { "ndarray": "zlanht.ndarray" }
