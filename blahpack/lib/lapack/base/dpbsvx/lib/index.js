
'use strict';

/**
* Solves a real system A * X = B where A is symmetric positive definite band, with equilibration, condition estimation, and error bounds.
*
* @module @stdlib/lapack/base/dpbsvx
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dpbsvx = require( '@stdlib/lapack/base/dpbsvx' );
*
* var AB = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 0.0, 6.0, 0.0, 0.0 ] );
* var AFB = new Float64Array( 9 );
* var S = new Float64Array( 3 );
* var equed = [ 'none' ];
* var B = new Float64Array( [ 7.0, 10.0, 10.0 ] );
* var X = new Float64Array( 3 );
* var rcond = new Float64Array( 1 );
* var FERR = new Float64Array( 1 );
* var BERR = new Float64Array( 1 );
* var WORK = new Float64Array( 9 );
* var IWORK = new Int32Array( 3 );
*
* var info = dpbsvx.ndarray( 'not-factored', 'lower', 3, 2, 1, AB, 1, 3, 0, AFB, 1, 3, 0, equed, S, 1, 0, B, 1, 3, 0, X, 1, 3, 0, rcond, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dpbsvx.ndarray" }
