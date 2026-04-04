
'use strict';

/**
* Uses the LU factorization to compute the solution to a complex system of.
* linear equations where A is a tridiagonal matrix, with condition estimation
* and error bounds.
*
* @module @stdlib/lapack/base/zgtsvx
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var zgtsvx = require( '@stdlib/lapack/base/zgtsvx' );
*
* var dl = new Complex128Array( new Float64Array( [ 1, 0.5, 2, -1 ] ) );
* var d = new Complex128Array( new Float64Array( [ 4, 1, 5, 0, 3, -0.5 ] ) );
* var du = new Complex128Array( new Float64Array( [ -1, 0.5, 1, 1 ] ) );
* var dlf = new Complex128Array( 2 );
* var df = new Complex128Array( 3 );
* var duf = new Complex128Array( 2 );
* var du2 = new Complex128Array( 1 );
* var ipiv = new Int32Array( 3 );
* var b = new Complex128Array( new Float64Array( [ 3, 1.5, 8, 0.5, 5, -1.5 ] ) );
* var x = new Complex128Array( 3 );
* var rcond = new Float64Array( 1 );
* var ferr = new Float64Array( 1 );
* var berr = new Float64Array( 1 );
* var work = new Complex128Array( 6 );
* var rwork = new Float64Array( 3 );
*
* var info = zgtsvx.ndarray( 'not-factored', 'no-transpose', 3, 1, dl, 1, 0, d, 1, 0, du, 1, 0, dlf, 1, 0, df, 1, 0, duf, 1, 0, du2, 1, 0, ipiv, 1, 0, b, 1, 3, 0, x, 1, 3, 0, rcond, ferr, 1, 0, berr, 1, 0, work, 1, 0, rwork, 1, 0 );
* // info => 0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "zgtsvx.ndarray" }
