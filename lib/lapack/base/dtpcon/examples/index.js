
'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dtpcon = require( './../lib' );

// Upper triangular 3x3 identity matrix in packed storage:
var AP = new Float64Array( [ 1.0, 0.0, 1.0, 0.0, 0.0, 1.0 ] );
var RCOND = new Float64Array( 1 );
var WORK = new Float64Array( 9 );
var IWORK = new Int32Array( 3 );

var info = dtpcon( 'one-norm', 'upper', 'non-unit', 3, AP, RCOND, WORK, IWORK );
console.log( 'info:', info );
console.log( 'rcond:', RCOND[ 0 ] );
// => rcond: 1.0 (identity matrix is perfectly conditioned)
