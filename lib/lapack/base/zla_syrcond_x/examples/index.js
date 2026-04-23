
'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zsytrf = require( './../../zsytrf/lib/base.js' );
var zlaSyrcondX = require( './../lib' );

var N = 3;
var A = new Complex128Array([
	2.0,
	1.0,
	0.0,
	0.0,
	0.0,
	0.0,
	1.0,
	0.0,
	3.0,
	0.5,
	0.0,
	0.0,
	0.0,
	-1.0,
	1.0,
	1.0,
	4.0,
	0.0
]);
var AF = new Complex128Array( A );
var IPIV = new Int32Array( N );
var X = new Complex128Array( [ 1, 0, 1, 0, 1, 0 ] );
var WORK = new Complex128Array( 2 * N );
var RWORK = new Float64Array( N );
var rcond;

zsytrf( 'upper', N, AF, 1, N, 0, IPIV, 1, 0 );

rcond = zlaSyrcondX( 'column-major', 'upper', N, A, N, AF, N, IPIV, 1, X, 1, WORK, 1, RWORK, 1 );
console.log( rcond ); // eslint-disable-line no-console
