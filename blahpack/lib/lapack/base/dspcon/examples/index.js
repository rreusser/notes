'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsptrf = require( '../../dsptrf/lib' );
var dspcon = require( './../lib' );

// 3x3 symmetric positive definite matrix in upper packed storage:

// A = [[4, 1, 1], [1, 3, 1], [1, 1, 2]]

// Upper packed: A(1,1), A(1,2), A(2,2), A(1,3), A(2,3), A(3,3)
var AP = new Float64Array( [ 4.0, 1.0, 3.0, 1.0, 1.0, 2.0 ] );
var IPIV = new Int32Array( 3 );
var rcond = new Float64Array( 1 );
var WORK = new Float64Array( 6 );
var IWORK = new Int32Array( 3 );
var info;

// Factorize with dsptrf:
dsptrf.ndarray( 'upper', 3, AP, 1, 0, IPIV, 1, 0 );

// Estimate reciprocal condition number (anorm = 6.0, the 1-norm of A):
info = dspcon.ndarray( 'upper', 3, AP, 1, 0, IPIV, 1, 0, 6.0, rcond, WORK, 1, 0, IWORK, 1, 0 ); // eslint-disable-line max-len

console.log( 'info:', info ); // eslint-disable-line no-console
console.log( 'rcond:', rcond[ 0 ] ); // eslint-disable-line no-console
// => rcond ~ 0.177
