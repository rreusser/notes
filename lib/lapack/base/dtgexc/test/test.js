
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtgexc = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dtgexc, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dtgexc.ndarray, 'function', 'has ndarray method' );
});

test( 'dtgexc reorders generalized Schur form (column-major)', function t() {
	var WORK;
	var A;
	var B;
	var Q;
	var Z;
	var r;

	A = new Float64Array( [ 1.0, 0.0, 0.0, 0.5, 2.0, 0.0, 0.3, 0.4, 3.0 ] );
	B = new Float64Array( [ 1.0, 0.0, 0.0, 0.2, 1.5, 0.0, 0.1, 0.3, 2.0 ] );
	Q = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 ] );
	Z = new Float64Array( [ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 ] );
	WORK = new Float64Array( 28 );
	r = dtgexc( 'column-major', true, true, 3, A, 3, B, 3, Q, 3, Z, 3, 0, 2, WORK, 1, 28 ); // eslint-disable-line max-len
	assert.strictEqual( r.info, 0, 'info is zero' );
	assert.strictEqual( r.ilst, 2, 'ilst is 2' );
	assert.ok( A[ 0 ] !== 1.0, 'A is modified' );
});
