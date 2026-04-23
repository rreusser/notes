
'use strict';

/**
* Provides error bounds for the solution to a system with a real triangular band matrix.
*
* @module @stdlib/lapack/base/dtbrfs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dtbrfs = require( '@stdlib/lapack/base/dtbrfs' );
*
* var AB = new Float64Array( [ 0, 0, 2, 0, 1, 4, 3, 5, 6, 2, 1, 3 ] );
* var B = new Float64Array( [ 13.0, 31.0, 22.0, 12.0 ] );
* var X = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Float64Array( 12 );
* var IWORK = new Int32Array( 4 );
*
* var info = dtbrfs.ndarray( 'upper', 'no-transpose', 'non-unit', 4, 2, 1, AB, 1, 3, 0, B, 1, 4, 0, X, 1, 4, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtbrfs;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtbrfs = main;
} else {
	dtbrfs = tmp;
}


// EXPORTS //

module.exports = dtbrfs;

// exports: { "ndarray": "dtbrfs.ndarray" }
