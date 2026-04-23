'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsytf2rk = require( './../lib' ).ndarray;

var A = new Float64Array([
	4,
	1,
	-2,
	0.5,
	0,
	-3,
	1,
	2,
	0,
	0,
	5,
	-1,
	0,
	0,
	0,
	2
]);
var e = new Float64Array( 4 );
var ipiv = new Int32Array( 4 );

var info = dsytf2rk( 'lower', 4, A, 1, 4, 0, e, 1, 0, ipiv, 1, 0 );
console.log( 'info: ' + info ); // eslint-disable-line no-console
console.log( 'A: ' + A ); // eslint-disable-line no-console
console.log( 'e: ' + e ); // eslint-disable-line no-console
console.log( 'ipiv: ' + ipiv ); // eslint-disable-line no-console
