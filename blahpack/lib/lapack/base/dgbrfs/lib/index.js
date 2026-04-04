
'use strict';

/**
* Improves the computed solution to a real system A * X = B where A is general band and provides error bounds.
*
* @module @stdlib/lapack/base/dgbrfs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dgbrfs = require( '@stdlib/lapack/base/dgbrfs' );
*
* var AB = new Float64Array( [ 3.0 ] );
* var AFB = new Float64Array( [ 3.0 ] );
* var IPIV = new Int32Array( [ 0 ] );
* var B = new Float64Array( [ 5.0 ] );
* var X = new Float64Array( [ 5.0 / 3.0 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Float64Array( 3 );
* var IWORK = new Int32Array( 1 );
*
* var info = dgbrfs.ndarray( 'no-transpose', 1, 0, 0, 1, AB, 1, 1, 0, AFB, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dgbrfs.ndarray" }
