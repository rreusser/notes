
'use strict';

/**
* Improve the computed solution to a complex tridiagonal system and provide error bounds.
*
* @module @stdlib/lapack/base/zgtrfs
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zgtrfs = require( '@stdlib/lapack/base/zgtrfs' );
*
* var DL = new Complex128Array( [ 2, 1, 1, -1, 3, 0.5 ] );
* var d = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
* var DU = new Complex128Array( [ 1, 0.5, -1, 1, 2, 1 ] );
* var DLF = new Complex128Array( [ 2, 1 ] );
* var DF = new Complex128Array( [ 4, 1, 5, 2, 3, 1, 6, -1 ] );
* var DUF = new Complex128Array( [ 1, 0.5 ] );
* var DU2 = new Complex128Array( 4 );
* var IPIV = new Int32Array( 4 );
* var B = new Complex128Array( [ 5, 1.5, 6, 4, 6, 1, 9, -0.5 ] );
* var X = new Complex128Array( [ 1, 0, 1, 0, 1, 0, 1, 0 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Complex128Array( 8 );
* var RWORK = new Float64Array( 4 );
*
* var info = zgtrfs.ndarray( 'no-transpose', 4, 1, DL, 1, 0, d, 1, 0, DU, 1, 0, DLF, 1, 0, DF, 1, 0, DUF, 1, 0, DU2, 1, 0, IPIV, 1, 0, B, 1, 4, 0, X, 1, 4, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
* // returns 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var zgtrfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	zgtrfs = main;
} else {
	zgtrfs = tmp;
}


// EXPORTS //

module.exports = zgtrfs;

// exports: { "ndarray": "zgtrfs.ndarray" }
