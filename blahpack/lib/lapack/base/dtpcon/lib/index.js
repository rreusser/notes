
'use strict';

/**
* Estimates the reciprocal condition number of a real triangular matrix in packed storage.
*
* @module @stdlib/lapack/base/dtpcon
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dtpcon = require( '@stdlib/lapack/base/dtpcon' );
*
* var AP = new Float64Array( [ 1.0, 0.0, 1.0, 0.0, 0.0, 1.0 ] );
* var rcond = new Float64Array( 1 );
* var WORK = new Float64Array( 9 );
* var IWORK = new Int32Array( 3 );
*
* var info = dtpcon( 'one-norm', 'upper', 'non-unit', 3, AP, rcond, WORK, IWORK );
* // info => 0
* // rcond[ 0 ] => 1.0
*/

// MODULES //

var main = require( './main.js' );


// EXPORTS //

module.exports = main;

// exports: { "ndarray": "dtpcon.ndarray" }
