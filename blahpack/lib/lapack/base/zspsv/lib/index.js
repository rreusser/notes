
'use strict';

/**
* Computes the solution to a complex system of linear equations A * X = B where A is symmetric in packed storage.
*
* @module @stdlib/lapack/base/zspsv
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zspsv = require( '@stdlib/lapack/base/zspsv' );
*
* var AP = new Complex128Array( [ 3.0, 1.0, 1.0, -1.0, 4.0, 2.0 ] );
* var IPIV = new Int32Array( 2 );
* var B = new Complex128Array( [ 4.0, 0.0, 5.0, 1.0 ] );
*
* var info = zspsv.ndarray( 'upper', 2, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 2, 0 );
* // returns 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zspsv;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zspsv = main;
} else {
	zspsv = tmp;
}


// EXPORTS //

module.exports = zspsv;

// exports: { "ndarray": "zspsv.ndarray" }
