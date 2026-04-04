
'use strict';

/**
* Provides error bounds for the solution to a system with a complex triangular matrix.
*
* @module @stdlib/lapack/base/ztrrfs
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var ztrrfs = require( '@stdlib/lapack/base/ztrrfs' );
*
* var A = new Complex128Array( [ 2, 1, 0, 0, 0, 0, 1, 2, 4, 1, 0, 0, 3, 1, 5, 2, 6, 1 ] );
* var B = new Complex128Array( [ 6, 4, 9, 3, 6, 1 ] );
* var X = new Complex128Array( [ 1, 0, 1, 0, 1, 0 ] );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Complex128Array( 6 );
* var RWORK = new Float64Array( 3 );
*
* var info = ztrrfs.ndarray( 'upper', 'no-transpose', 'non-unit', 3, 1, A, 1, 3, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, RWORK, 1, 0 );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "ztrrfs.ndarray" }
