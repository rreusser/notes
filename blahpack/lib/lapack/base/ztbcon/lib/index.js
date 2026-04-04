
'use strict';

/**
* Estimates the reciprocal condition number of a complex triangular band matrix.
*
* @module @stdlib/lapack/base/ztbcon
*
* @example
* var Complex128Array = require( '@stdlib/array/complex128' );
* var Float64Array = require( '@stdlib/array/float64' );
* var ztbcon = require( '@stdlib/lapack/base/ztbcon' );
*
* // 3x3 identity band matrix (KD=0):
* var AB = new Complex128Array( [ 1, 0, 1, 0, 1, 0 ] );
* var RCOND = new Float64Array( 1 );
* var WORK = new Complex128Array( 6 );
* var RWORK = new Float64Array( 3 );
*
* var info = ztbcon.ndarray( 'one-norm', 'upper', 'non-unit', 3, 0, AB, 1, 1, 0, RCOND, WORK, 1, 0, RWORK, 1, 0 );
* // info => 0
* // RCOND[ 0 ] => 1.0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "ztbcon.ndarray" }
