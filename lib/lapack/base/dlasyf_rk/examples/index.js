'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlasyfRk = require( './../lib' );

var N = 4;
var nb = 4;
var A = new Float64Array([
	4.0,
	1.0,
	2.0,
	0.5,
	0.0,
	3.0,
	0.5,
	1.0,
	0.0,
	0.0,
	5.0,
	0.2,
	0.0,
	0.0,
	0.0,
	6.0
]);
var e = new Float64Array( N );
var IPIV = new Int32Array( N );
var W = new Float64Array( N * nb );

var result = dlasyfRk( 'column-major', 'lower', N, nb, A, N, e, IPIV, W, N );
console.log( result ); // eslint-disable-line no-console
console.log( A ); // eslint-disable-line no-console
