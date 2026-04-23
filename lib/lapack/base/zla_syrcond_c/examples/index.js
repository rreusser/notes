/* eslint-disable camelcase */

'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var zsytrf = require( './../../zsytrf/lib/base.js' );
var zla_syrcond_c = require( './../lib/base.js' );

var N = 3;
var A = new Complex128Array([
	2.0,
	1.0,
	1.0,
	0.5,
	0.5,
	-0.5,
	1.0,
	0.5,
	3.0,
	-0.5,
	-0.5,
	0.25,
	0.5,
	-0.5,
	-0.5,
	0.25,
	4.0,
	0.0
]);
var AF = new Complex128Array( A.length );
var IPIV = new Int32Array( N );
var C = new Float64Array([ 1.0, 2.0, 0.5 ]);
var WORK = new Complex128Array( 2 * N );
var RWORK = new Float64Array( N );
var rcond;
var i;

for ( i = 0; i < A.length; i++ ) {
	AF.set( A.get( i ), i );
}

zsytrf( 'upper', N, AF, 1, N, 0, IPIV, 1, 0 );

rcond = zla_syrcond_c( 'upper', N, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, C, 1, 0, true, WORK, 1, 0, RWORK, 1, 0 );
console.log( rcond ); // eslint-disable-line no-console
