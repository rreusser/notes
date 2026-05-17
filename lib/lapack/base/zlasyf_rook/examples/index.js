'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zlasyfRook = require( './../lib' );

var N = 3;
var nb = 3;

// 3x3 complex symmetric matrix in column-major (lower triangle filled):
var A = new Complex128Array([
	4.0,
	0.2,
	1.0,
	0.5,
	2.0,
	-1.0,
	0.0,
	0.0,
	3.0,
	-0.1,
	0.5,
	-0.2,
	0.0,
	0.0,
	0.0,
	0.0,
	5.0,
	0.3
]);
var IPIV = new Int32Array( N );
var W = new Complex128Array( N * nb );

var result = zlasyfRook( 'column-major', 'lower', N, nb, A, N, IPIV, W, N );
console.log( result ); // eslint-disable-line no-console
