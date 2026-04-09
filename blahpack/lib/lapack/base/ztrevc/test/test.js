
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var Uint8Array = require( '@stdlib/array/uint8' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ztrevc = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof ztrevc, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof ztrevc.ndarray, 'function', 'has ndarray method' );
});

test( 'computes right eigenvectors of a 1x1 matrix', function t() {
	var RWORK;
	var WORK;
	var info;
	var vrv;
	var VR;
	var VL;
	var T;

	RWORK = new Float64Array( 1 );
	WORK = new Complex128Array( 2 );
	VR = new Complex128Array( 1 );
	VL = new Complex128Array( 1 );
	T = new Complex128Array( [ 5, 2 ] );
	info = ztrevc( 'column-major', 'right', 'all', new Uint8Array( 1 ), 1, 1, T, 1, VL, 1, VR, 1, 1, 0, WORK, 1, RWORK, 1 ); // eslint-disable-line max-len
	vrv = reinterpret( VR, 0 );
	assert.strictEqual( info, 0, 'returns 0' );
	assert.strictEqual( vrv[ 0 ], 1.0, 'VR[0] real is 1' );
	assert.strictEqual( vrv[ 1 ], 0.0, 'VR[0] imag is 0' );
});
