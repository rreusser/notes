'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Int32Array = require( '@stdlib/array/int32' );
var zlasyfRk = require( './../lib' );

var N = 3;
var nb = 3;
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
var e = new Complex128Array( N );
var IPIV = new Int32Array( N );
var W = new Complex128Array( N * nb );

var result = zlasyfRk( 'column-major', 'lower', N, nb, A, N, e, IPIV, W, N );
console.log( result ); // eslint-disable-line no-console
