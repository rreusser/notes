'use strict';

/**
* Returns the norm of a real triangular band matrix.
*
* @module @stdlib/lapack/base/dlantb
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dlantb = require( '@stdlib/lapack/base/dlantb' );
*
* // Upper triangular 4x4 band matrix with K=1:
* var AB = new Float64Array( [ 0, 2, -3, 4, 1, -5, 6, 7 ] );
* var WORK = new Float64Array( 4 );
*
* var result = dlantb.ndarray( 'one-norm', 'upper', 'non-unit', 4, 1, AB, 1, 2, 0, WORK, 1, 0 );
* // returns 13.0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dlantb.ndarray" }
