/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zppcon = require( './../lib' );

var anorm = 5.41421356237309492;
var WORK = new Complex128Array( 6 );
var RWORK = new Float64Array( 3 );
var rcond = new Float64Array( 1 );
var info;
var AP;

// 3x3 HPD matrix factored by zpptrf, upper packed:
AP = new Complex128Array([
	2.0,
	0.0,
	0.5,
	0.5,
	1.58113883008418976,
	0.0,
	0.0,
	0.0,
	0.6324555320336759,
	0.0,
	1.2649110640673518,
	0.0
]);

info = zppcon.ndarray( 'upper', 3, AP, 1, 0, anorm, rcond, WORK, 1, 0, RWORK, 1, 0 ); // eslint-disable-line max-len

console.log( 'info: %d', info ); // eslint-disable-line no-console
console.log( 'rcond: %d', rcond[ 0 ] ); // eslint-disable-line no-console
