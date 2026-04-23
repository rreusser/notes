
'use strict';

/**
* Improves the computed solution to a complex system A * X = B where A is Hermitian in packed storage and provides error bounds.
*
* @module @stdlib/lapack/base/zhprfs
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zhprfs = require( '@stdlib/lapack/base/zhprfs' );
*
* var AP = new Complex128Array( [ 4.0, 0.0, 1.0, -1.0, 5.0, 0.0 ] );
* var AFP = new Complex128Array( [ 4.0, 0.0, 1.0, -1.0, 5.0, 0.0 ] );
* var IPIV = new Int32Array( [ 0, 1 ] );
* var B = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0 ] );
* var X = new Complex128Array( [ 0.25, 0.0, 0.375, 0.0 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
*
* var info = zhprfs.ndarray( 'upper', 2, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 2, 0, X, 1, 2, 0, FERR, 1, 0, BERR, 1, 0, new Complex128Array( 4 ), 1, 0, new Float64Array( 2 ), 1, 0 );
* // returns 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zhprfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zhprfs = main;
} else {
	zhprfs = tmp;
}


// EXPORTS //

module.exports = zhprfs;

// exports: { "ndarray": "zhprfs.ndarray" }
