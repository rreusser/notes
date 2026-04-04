/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zhbevx = require( './../lib' );

// 4x4 Hermitian tridiagonal matrix (KD=1), lower band storage:

//   4      (1+i)   0      0

//  (1-i)    5     (2-i)   0

//   0      (2+i)   6     (3+i)

//   0       0     (3-i)   7
var AB = new Complex128Array([
	4.0,
	0.0,
	1.0,
	-1.0,
	5.0,
	0.0,
	2.0,
	1.0,
	6.0,
	0.0,
	3.0,
	-1.0,
	7.0,
	0.0,
	0.0,
	0.0
]);
var Q = new Complex128Array( 16 );
var W = new Float64Array( 4 );
var Z = new Complex128Array( 16 );
var WORK = new Complex128Array( 20 );
var RWORK = new Float64Array( 50 );
var IWORK = new Int32Array( 30 );
var IFAIL = new Int32Array( 4 );
var out = {
	'M': 0
};

// Compute all eigenvalues and eigenvectors:
var info = zhbevx.ndarray( 'compute-vectors', 'all', 'lower', 4, 1, AB, 1, 2, 0, Q, 1, 4, 0, 0, 0, 0, 0, 0, out, W, 1, 0, Z, 1, 4, 0, WORK, 1, 0, RWORK, 1, 0, IWORK, 1, 0, IFAIL, 1, 0 ); // eslint-disable-line max-len

console.log( 'info:', info );
console.log( 'M (eigenvalues found):', out.M );
console.log( 'Eigenvalues (W):', W );
