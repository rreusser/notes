
'use strict';

/**
* Improves the computed solution to a real system A * X = B where A is symmetric positive definite in packed storage and provides error bounds.
*
* @module @stdlib/lapack/base/dpprfs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dpprfs = require( '@stdlib/lapack/base/dpprfs' );
*
* var AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
* var AFP = new Float64Array( [ 2.0, 1.0, 2.0, 0.5, 1.25, 2.0 ] ); // pre-factored
* var B = new Float64Array( [ 1.0, 1.0, 1.0 ] );
* var X = new Float64Array( [ 0.194, 0.0597, 0.1045 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Float64Array( 9 );
* var IWORK = new Int32Array( 3 );
*
* var info = dpprfs( 'upper', 3, 1, AP, AFP, B, 3, X, 3, FERR, BERR, WORK, IWORK );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dpprfs.ndarray" }
