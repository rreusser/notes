'use strict';

/**
* Solves a real symmetric positive definite tridiagonal system A_X = B, and provides an estimate of the condition number and error bounds on the solution.
*
* @module @stdlib/lapack/base/dptsvx
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var dptsvx = require( '@stdlib/lapack/base/dptsvx' );
*
* var d = new Float64Array( [ 4.0, 5.0, 6.0, 7.0 ] );
* var e = new Float64Array( [ 1.0, 2.0, 3.0 ] );
* var df = new Float64Array( 4 );
* var ef = new Float64Array( 3 );
* var b = new Float64Array( [ 5.0, 8.0, 11.0, 10.0 ] );
* var x = new Float64Array( 4 );
* var rcond = new Float64Array( 1 );
* var ferr = new Float64Array( 1 );
* var berr = new Float64Array( 1 );
* var work = new Float64Array( 8 );
*
* var info = dptsvx.ndarray( 'not-factored', 4, 1, d, 1, 0, e, 1, 0, df, 1, 0, ef, 1, 0, b, 1, 4, 0, x, 1, 4, 0, rcond, ferr, 1, 0, berr, 1, 0, work, 1, 0 );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dptsvx.ndarray" }
