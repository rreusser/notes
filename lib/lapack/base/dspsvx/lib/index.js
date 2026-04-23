
'use strict';

/**
* Solves a real system A * X = B where A is symmetric in packed storage, with condition estimation and error bounds.
*
* @module @stdlib/lapack/base/dspsvx
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dspsvx = require( '@stdlib/lapack/base/dspsvx' );
*
* var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
* var AFP = new Float64Array( 6 );
* var IPIV = new Int32Array( 3 );
* var B = new Float64Array( [ 7.0, 10.0, 10.0 ] );
* var X = new Float64Array( 3 );
* var rcond = new Float64Array( 1 );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Float64Array( 9 );
* var IWORK = new Int32Array( 3 );
*
* var info = dspsvx.ndarray( 'not-factored', 'upper', 3, 1, AP, 1, 0, AFP, 1, 0, IPIV, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
* // returns 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dspsvx;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dspsvx = main;
} else {
	dspsvx = tmp;
}


// EXPORTS //

module.exports = dspsvx;

// exports: { "ndarray": "dspsvx.ndarray" }
