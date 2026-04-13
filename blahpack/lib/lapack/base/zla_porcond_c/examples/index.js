
/* eslint-disable camelcase */

'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zpotrf = require( './../../zpotrf/lib/base.js' );
var zla_porcond_c = require( './../lib/base.js' );

var N = 3;
var A = new Complex128Array([
	// Column 1 of a 3x3 Hermitian positive-definite matrix
	6.0,
	0.0,
	1.0,
	-1.0,
	0.0,
	1.0,

	// Column 2
	1.0,
	1.0,
	7.0,
	0.0,
	2.0,
	-0.5,

	// Column 3
	0.0,
	-1.0,
	2.0,
	0.5,
	8.0,
	0.0
]);
var AF = new Complex128Array( A.length );
var C = new Float64Array([ 2.0, 0.5, 3.0 ]);
var WORK = new Complex128Array( 2 * N );
var RWORK = new Float64Array( N );
var rcond;
var i;

for ( i = 0; i < A.length; i++ ) {
	AF.set( A.get( i ), i );
}

zpotrf( 'upper', N, AF, 1, N, 0 );

rcond = zla_porcond_c( 'upper', N, A, 1, N, 0, AF, 1, N, 0, C, 1, 0, true, WORK, 1, 0, RWORK, 1, 0 );
console.log( rcond ); // eslint-disable-line no-console
