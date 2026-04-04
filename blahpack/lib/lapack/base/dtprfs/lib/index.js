
'use strict';

/**
* Provides error bounds for the solution to a system with a real triangular matrix in packed storage.
*
* @module @stdlib/lapack/base/dtprfs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dtprfs = require( '@stdlib/lapack/base/dtprfs' );
*
* var AP = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
* var B = new Float64Array( [ 13.0, 23.0, 18.0 ] );
* var X = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Float64Array( 9 );
* var IWORK = new Int32Array( 3 );
*
* var info = dtprfs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, AP, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtprfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtprfs = main;
} else {
	dtprfs = tmp;
}


// EXPORTS //

module.exports = dtprfs;

// exports: { "ndarray": "dtprfs.ndarray" }
