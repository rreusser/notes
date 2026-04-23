
'use strict';

/**
* Computes the solution to a complex system of linear equations A * X = B where A is Hermitian in packed storage.
*
* @module @stdlib/lapack/base/zhpsv
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zhpsv = require( '@stdlib/lapack/base/zhpsv' );
*
* var AP = new Complex128Array( [ 4, 0, 1, 2, 5, 0, 2, -1, 3, 1, 6, 0 ] );
* var B = new Complex128Array( [ 1, 0, 0, 1, 1, -1 ] );
* var IPIV = new Int32Array( 3 );
*
* var info = zhpsv( 'upper', 3, 1, AP, IPIV, B, 3 );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhpsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhpsv = main;
} else {
	zhpsv = tmp;
}


// EXPORTS //

module.exports = zhpsv;

// exports: { "ndarray": "zhpsv.ndarray" }
