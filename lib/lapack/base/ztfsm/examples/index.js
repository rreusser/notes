
'use strict';

var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztfsm = require( './../lib' );

var alpha;
var A;
var B;
var Bv;

// 3x3 lower triangular matrix in RFP format (TRANSR='N', UPLO='L'):
A = new Complex128Array([
	4,
	0,
	2,
	1,
	3,
	1.5,
	9,
	0,
	7,
	0,
	5,
	2.5
]);

// B is a 3x1 matrix:
B = new Complex128Array([
	1,
	0.3,
	2,
	0.6,
	3,
	0.9
]);
alpha = new Complex128( 1.0, 0.0 );

// Solve op(A) * X = alpha * B with A lower-triangular:
ztfsm( 'no-transpose', 'left', 'lower', 'no-transpose', 'non-unit', 3, 1, alpha, A, B ); // eslint-disable-line max-len

Bv = reinterpret( B, 0 );
console.log( 'X (solution):', Array.prototype.slice.call( Bv ) ); // eslint-disable-line no-console
