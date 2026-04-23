'use strict';

/**
* Estimates the reciprocal condition number of a real triangular band matrix.
*
* @module @stdlib/lapack/base/dtbcon
*
* @example
* var Float64Array = require( '@stdlib/array/float64' );
* var Int32Array = require( '@stdlib/array/int32' );
* var dtbcon = require( '@stdlib/lapack/base/dtbcon' );
*
* // 3x3 upper triangular band matrix with kd=1 (column-major band storage):
* var AB = new Float64Array( [ 0.0, 2.0, -1.0, 3.0, -1.0, 4.0 ] );
* var rcond = new Float64Array( 1 );
* var WORK = new Float64Array( 9 );
* var IWORK = new Int32Array( 3 );
*
* var info = dtbcon( 'one-norm', 'upper', 'non-unit', 3, 1, AB, 2, rcond, WORK, IWORK );
* // info => 0
*/

// MODULES //

var join = require( 'path' ).join;
var tryRequire = require( '@stdlib/utils/try-require' );
var isError = require( '@stdlib/assert/is-error' );
var main = require( './main.js' );


// MAIN //

var dtbcon;
var tmp = tryRequire( join( __dirname, './native.js' ) );
if ( isError( tmp ) ) {
	dtbcon = main;
} else {
	dtbcon = tmp;
}


// EXPORTS //

module.exports = dtbcon;

// exports: { "ndarray": "dtbcon.ndarray" }
