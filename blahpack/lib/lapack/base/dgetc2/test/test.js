/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgetc2 = require( './../lib/base.js' );

// FIXTURES //

var basic_2x2 = require( './fixtures/basic_2x2.json' );
var basic_3x3 = require( './fixtures/basic_3x3.json' );
var basic_4x4 = require( './fixtures/basic_4x4.json' );
var n_equals_1 = require( './fixtures/n_equals_1.json' );
var near_singular = require( './fixtures/near_singular.json' );

// FUNCTIONS //

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

// TESTS //

test( 'dgetc2: basic 2x2', function t() {
	var IPIV;
	var JPIV;
	var info;
	var tc;
	var A;
	var i;

	tc = basic_2x2;
	A = new Float64Array( [ 1.0, 3.0, 2.0, 4.0 ] );
	IPIV = new Int32Array( 2 );
	JPIV = new Int32Array( 2 );
	info = dgetc2( 2, A, 1, 2, 0, IPIV, 1, 0, JPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.prototype.slice.call( A ), tc.A, 1e-14, 'A' );
	for ( i = 0; i < 2; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
		assert.strictEqual( JPIV[ i ], tc.jpiv[ i ] - 1, 'jpiv[' + i + ']' );
	}
});

test( 'dgetc2: basic 3x3', function t() {
	var IPIV;
	var JPIV;
	var info;
	var tc;
	var A;
	var i;

	tc = basic_3x3;
	A = new Float64Array([
		1.0,
		4.0,
		7.0,
		2.0,
		5.0,
		8.0,
		3.0,
		6.0,
		10.0
	]);
	IPIV = new Int32Array( 3 );
	JPIV = new Int32Array( 3 );
	info = dgetc2( 3, A, 1, 3, 0, IPIV, 1, 0, JPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.prototype.slice.call( A ), tc.A, 1e-14, 'A' );
	for ( i = 0; i < 3; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
		assert.strictEqual( JPIV[ i ], tc.jpiv[ i ] - 1, 'jpiv[' + i + ']' );
	}
});

test( 'dgetc2: basic 4x4 with complete pivoting', function t() {
	var IPIV;
	var JPIV;
	var info;
	var tc;
	var A;
	var i;

	tc = basic_4x4;
	A = new Float64Array([
		0.1,
		0.4,
		0.8,
		1.2,
		0.2,
		0.5,
		0.9,
		1.3,
		0.3,
		0.6,
		1.0,
		1.4,
		10.0,
		0.7,
		1.1,
		1.5
	]);
	IPIV = new Int32Array( 4 );
	JPIV = new Int32Array( 4 );
	info = dgetc2( 4, A, 1, 4, 0, IPIV, 1, 0, JPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.prototype.slice.call( A ), tc.A, 1e-14, 'A' );
	for ( i = 0; i < 4; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
		assert.strictEqual( JPIV[ i ], tc.jpiv[ i ] - 1, 'jpiv[' + i + ']' );
	}
});

test( 'dgetc2: N=1', function t() {
	var IPIV;
	var JPIV;
	var info;
	var tc;
	var A;

	tc = n_equals_1;
	A = new Float64Array( [ 5.0 ] );
	IPIV = new Int32Array( 1 );
	JPIV = new Int32Array( 1 );
	info = dgetc2( 1, A, 1, 1, 0, IPIV, 1, 0, JPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.prototype.slice.call( A ), tc.A, 1e-14, 'A' );
	assert.strictEqual( IPIV[ 0 ], tc.ipiv[ 0 ] - 1, 'ipiv[0]' );
	assert.strictEqual( JPIV[ 0 ], tc.jpiv[ 0 ] - 1, 'jpiv[0]' );
});

test( 'dgetc2: near-singular', function t() {
	var IPIV;
	var JPIV;
	var info;
	var tc;
	var A;
	var i;

	tc = near_singular;
	A = new Float64Array( [ 1e-200, 1.0, 1.0, 1.0 ] );
	IPIV = new Int32Array( 2 );
	JPIV = new Int32Array( 2 );
	info = dgetc2( 2, A, 1, 2, 0, IPIV, 1, 0, JPIV, 1, 0 );
	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.prototype.slice.call( A ), tc.A, 1e-14, 'A' );
	for ( i = 0; i < 2; i++ ) {
		assert.strictEqual( IPIV[ i ], tc.ipiv[ i ] - 1, 'ipiv[' + i + ']' );
		assert.strictEqual( JPIV[ i ], tc.jpiv[ i ] - 1, 'jpiv[' + i + ']' );
	}
});

test( 'dgetc2: N=0', function t() {
	var IPIV;
	var JPIV;
	var info;
	var A;

	A = new Float64Array( 1 );
	IPIV = new Int32Array( 1 );
	JPIV = new Int32Array( 1 );
	info = dgetc2( 0, A, 1, 1, 0, IPIV, 1, 0, JPIV, 1, 0 );
	assert.strictEqual( info, 0, 'info' );
});
