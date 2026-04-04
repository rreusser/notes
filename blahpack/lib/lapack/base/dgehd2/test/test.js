/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgehd2 = require( './../lib/base.js' );

// FIXTURES //

var _4x4_full = require( './fixtures/4x4_full.json' );
var _5x5_full = require( './fixtures/5x5_full.json' );
var _4x4_partial_ilo2_ihi3 = require( './fixtures/4x4_partial_ilo2_ihi3.json' );
var n_one = require( './fixtures/n_one.json' );
var n_two = require( './fixtures/n_two.json' );
var ilo_eq_ihi = require( './fixtures/ilo_eq_ihi.json' );

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
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
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

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}

// TESTS //

test( 'dgehd2: main export is a function', function t() {
	assert.strictEqual( typeof dgehd2, 'function' );
});

test( 'dgehd2: 4x4 full range (ILO=1, IHI=4)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var N;
	var A;

	tc = _4x4_full;
	N = 4;
	A = new Float64Array([
		1,
		5,
		9,
		13,
		2,
		6,
		10,
		14,
		3,
		7,
		11,
		15,
		4,
		8,
		12,
		16
	]);
	TAU = new Float64Array( N - 1 );
	WORK = new Float64Array( N );
	info = dgehd2( N, 1, N, A, 1, N, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( toArray( A ), tc.A, 1e-10, 'A' );
	assertArrayClose( toArray( TAU ), tc.TAU, 1e-10, 'TAU' );
});

test( 'dgehd2: 5x5 full range', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var N;
	var A;

	tc = _5x5_full;
	N = 5;
	A = new Float64Array([
		2,
		1,
		3,
		1,
		4,
		1,
		4,
		1,
		2,
		1,
		3,
		1,
		5,
		1,
		2,
		1,
		2,
		1,
		6,
		1,
		4,
		1,
		2,
		1,
		7
	]);
	TAU = new Float64Array( N - 1 );
	WORK = new Float64Array( N );
	info = dgehd2( N, 1, N, A, 1, N, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( toArray( A ), tc.A, 1e-10, 'A' );
	assertArrayClose( toArray( TAU ), tc.TAU, 1e-10, 'TAU' );
});

test( 'dgehd2: 4x4 partial range (ILO=2, IHI=3)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var N;
	var A;

	tc = _4x4_partial_ilo2_ihi3;
	N = 4;
	A = new Float64Array([
		1,
		0,
		0,
		0,
		2,
		5,
		8,
		0,
		3,
		6,
		9,
		0,
		4,
		7,
		10,
		11
	]);
	TAU = new Float64Array( N - 1 );
	WORK = new Float64Array( N );
	info = dgehd2( N, 2, 3, A, 1, N, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( toArray( A ), tc.A, 1e-10, 'A' );
	assertArrayClose( toArray( TAU ), tc.TAU, 1e-10, 'TAU' );
});

test( 'dgehd2: N=1 (quick return)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = n_one;
	A = new Float64Array( [ 42.0 ] );
	TAU = new Float64Array( 0 );
	WORK = new Float64Array( 1 );
	info = dgehd2( 1, 1, 1, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertClose( A[ 0 ], tc.A[ 0 ], 1e-14, 'A[0]' );
});

test( 'dgehd2: N=2', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = n_two;
	A = new Float64Array( [ 3, 4, 1, 2 ] );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 2 );
	info = dgehd2( 2, 1, 2, A, 1, 2, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( toArray( A ), tc.A, 1e-10, 'A' );
	assertArrayClose( toArray( TAU ), tc.TAU, 1e-10, 'TAU' );
});

test( 'dgehd2: ILO=IHI (nothing to reduce)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var N;
	var A;

	tc = ilo_eq_ihi;
	N = 4;
	A = new Float64Array([
		1,
		0,
		0,
		0,
		2,
		5,
		0,
		0,
		3,
		6,
		9,
		0,
		4,
		7,
		10,
		11
	]);
	TAU = new Float64Array( N - 1 );
	WORK = new Float64Array( N );
	info = dgehd2( N, 2, 2, A, 1, N, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( toArray( A ), tc.A, 1e-10, 'A' );
});
