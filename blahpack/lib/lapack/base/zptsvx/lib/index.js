'use strict';

/**
* Solves a complex Hermitian positive definite tridiagonal system A*X = B, and provides an estimate of the condition number and error bounds on the solution.
*
* @module @stdlib/lapack/base/zptsvx
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Complex128Array = require( '@stdlib/array/complex128' );
* var zptsvx = require( '@stdlib/lapack/base/zptsvx' );
*
* var d = new Float64Array( [ 4.0, 5.0, 6.0, 7.0 ] );
* var e = new Complex128Array( [ 1.0, 0.5, 0.5, -0.3, 0.2, 0.1 ] );
* var df = new Float64Array( 4 );
* var ef = new Complex128Array( 3 );
* var b = new Complex128Array( [ 6.5, 3.5, 13.15, -5.7, 6.91, 3.22, -3.3, 7.15 ] );
* var x = new Complex128Array( 4 );
* var rcond = new Float64Array( 1 );
* var ferr = new Float64Array( 1 );
* var berr = new Float64Array( 1 );
* var work = new Complex128Array( 4 );
* var rwork = new Float64Array( 4 );
*
* var info = zptsvx.ndarray( 'not-factored', 4, 1, d, 1, 0, e, 1, 0, df, 1, 0, ef, 1, 0, b, 1, 4, 0, x, 1, 4, 0, rcond, ferr, 1, 0, berr, 1, 0, work, 1, 0, rwork, 1, 0 );
* // returns 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zptsvx.ndarray" }
