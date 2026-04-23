
'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zgetrf = require( './../../zgetrf/lib/base.js' );
var zlaGercondX = require( './../lib' );

var N = 3;
var A = new Complex128Array([
	2,
	1,
	1,
	-1,
	0,
	1,
	1,
	0,
	3,
	0,
	1,
	-0.5,
	0,
	-1,
	1,
	1,
	4,
	0
]);
var AF = new Complex128Array( A );
var IPIV = new Int32Array( N );
var X = new Complex128Array( [ 1, 0, 1, 0, 1, 0 ] );
var WORK = new Complex128Array( 2 * N );
var RWORK = new Float64Array( N );
var rcond;

zgetrf( N, N, AF, 1, N, 0, IPIV, 1, 0 );

rcond = zlaGercondX( 'column-major', 'no-transpose', N, A, N, AF, N, IPIV, 1, 0, X, 1, WORK, 1, RWORK, 1 );
console.log( rcond ); // eslint-disable-line no-console
