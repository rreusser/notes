
'use strict';

/**
* Provides error bounds for the solution to a system with a complex triangular matrix in packed storage.
*
* @module @stdlib/lapack/base/ztprfs
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Complex128Array = require( '@stdlib/array/complex128' );
* var ztprfs = require( '@stdlib/lapack/base/ztprfs' );
*
* var AP = new Complex128Array( [ 2.0, 1.0, 1.0, 0.5, 4.0, -1.0, 3.0, 2.0, 5.0, 0.0, 6.0, -0.5 ] );
* var B = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0 ] );
* var X = new Complex128Array( [ 1.0, 0.0, 2.0, 0.0, 3.0, 0.0 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Complex128Array( 6 );
* var RWORK = new Float64Array( 3 );
*
* var info = ztprfs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, AP, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "ztprfs.ndarray" }
