/* eslint-disable camelcase, stdlib/require-file-extensions */

'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgbtrf = require( '@stdlib/lapack/base/dgbtrf' );
var dla_gbrcond = require( '@stdlib/lapack/base/dla_gbrcond' );

// 3x3 tridiagonal matrix (KL=1, KU=1):

// [ 2  3  0 ]

// [ 1  5  6 ]

// [ 0  4  8 ]
var AB = new Float64Array( [ 0.0, 2.0, 1.0, 3.0, 5.0, 4.0, 6.0, 8.0, 0.0 ] );
var AFB = new Float64Array([
	0.0, 0.0, 2.0, 1.0, 0.0, 3.0, 5.0, 4.0, 0.0, 6.0, 8.0, 0.0
]);
var IPIV = new Int32Array( 3 );
var c = new Float64Array( [ 1.0, 2.0, 3.0 ] );
var WORK = new Float64Array( 15 );
var IWORK = new Int32Array( 3 );

var result;

dgbtrf.ndarray( 3, 3, 1, 1, AFB, 1, 4, 0, IPIV, 1, 0 );

result = dla_gbrcond.ndarray('no-transpose', 3, 1, 1, AB, 1, 3, 0, AFB, 1, 4, 0, IPIV, 1, 0, 1, c, 1, 0, WORK, 1, 0, IWORK, 1, 0);
console.log( 'Skeel condition number:', result ); // eslint-disable-line no-console
