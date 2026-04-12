
/* eslint-disable camelcase */

'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zgetrf = require( './../../zgetrf/lib/base.js' );
var zla_gercond_c = require( './../lib/base.js' );

var N = 3;
var A = new Complex128Array([
	2.0,
	1.0,
	1.0,
	2.0,
	0.0,
	1.0,
	1.0,
	0.0,
	3.0,
	0.0,
	1.0,
	-1.0,
	0.0,
	-1.0,
	1.0,
	1.0,
	4.0,
	0.0
]);
var AF = new Complex128Array( A.length );
var IPIV = new Int32Array( N );
var C = new Float64Array([ 2.0, 0.5, 3.0 ]);
var WORK = new Complex128Array( 2 * N );
var RWORK = new Float64Array( N );
var rcond;
var i;

for ( i = 0; i < A.length; i++ ) {
	AF.set( A.get( i ), i );
}

zgetrf( N, N, AF, 1, N, 0, IPIV, 1, 0 );

rcond = zla_gercond_c( 'no-transpose', N, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, C, 1, 0, true, WORK, 1, 0, RWORK, 1, 0 );
console.log( rcond ); // eslint-disable-line no-console
