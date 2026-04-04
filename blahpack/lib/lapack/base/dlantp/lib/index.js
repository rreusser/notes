
'use strict';

/**
* Returns the value of the one-norm, Frobenius norm, infinity-norm, or the largest absolute value of any element of a real triangular matrix supplied in packed form.
*
* @module @stdlib/lapack/base/dlantp
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlantp = require( '@stdlib/lapack/base/dlantp' );
*
* var AP = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = dlantp( 'max', 'upper', 'non-unit', 3, AP, WORK );
* // returns 7.0
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlantp = require( '@stdlib/lapack/base/dlantp' );
*
* var AP = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
* var WORK = new Float64Array( 3 );
*
* var result = dlantp.ndarray( 'max', 'upper', 'non-unit', 3, AP, 1, 0, WORK, 1, 0 );
* // returns 7.0
*/


// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dlantp.ndarray" }
