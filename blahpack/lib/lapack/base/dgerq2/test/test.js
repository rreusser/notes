/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgerq2 = require( './../lib/base.js' );

// FIXTURES //

var _3x4 = require( './fixtures/3x4.json' );
var _4x3 = require( './fixtures/4x3.json' );
var _3x3 = require( './fixtures/3x3.json' );
var _1x4 = require( './fixtures/1x4.json' );
var _3x1 = require( './fixtures/3x1.json' );
var _1x1 = require( './fixtures/1x1.json' );
var _2x5 = require( './fixtures/2x5.json' );

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

/**
* ExtractMatrix.
*
* @private
* @param {*} A - A
* @param {*} LDA - LDA
* @param {*} M - M
* @param {*} N - N
* @returns {*} result
*/
function extractMatrix( A, LDA, M, N ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( A[ j * LDA + i ] );
		}
	}
	return out;
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

test( 'dgerq2: 3x4 (M < N)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = _3x4;
	A = new Float64Array( 6 * 4 );
	A[ 0 * 6 + 0 ] = 2.0;
	A[ 1 * 6 + 0 ] = 1.0;
	A[ 2 * 6 + 0 ] = 3.0;
	A[ 3 * 6 + 0 ] = 1.0;
	A[ 0 * 6 + 1 ] = 1.0;
	A[ 1 * 6 + 1 ] = 4.0;
	A[ 2 * 6 + 1 ] = 2.0;
	A[ 3 * 6 + 1 ] = 3.0;
	A[ 0 * 6 + 2 ] = 3.0;
	A[ 1 * 6 + 2 ] = 2.0;
	A[ 2 * 6 + 2 ] = 5.0;
	A[ 3 * 6 + 2 ] = 2.0;
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 3 );
	info = dgerq2( 3, 4, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 3, 4 ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgerq2: 4x3 (M > N)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = _4x3;
	A = new Float64Array( 6 * 3 );
	A[ 0 * 6 + 0 ] = 2.0;
	A[ 1 * 6 + 0 ] = 1.0;
	A[ 2 * 6 + 0 ] = 3.0;
	A[ 0 * 6 + 1 ] = 1.0;
	A[ 1 * 6 + 1 ] = 4.0;
	A[ 2 * 6 + 1 ] = 2.0;
	A[ 0 * 6 + 2 ] = 3.0;
	A[ 1 * 6 + 2 ] = 2.0;
	A[ 2 * 6 + 2 ] = 5.0;
	A[ 0 * 6 + 3 ] = 1.0;
	A[ 1 * 6 + 3 ] = 3.0;
	A[ 2 * 6 + 3 ] = 1.0;
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 4 );
	info = dgerq2( 4, 3, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 4, 3 ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgerq2: 3x3 (square)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = _3x3;
	A = new Float64Array( 6 * 3 );
	A[ 0 * 6 + 0 ] = 4.0;
	A[ 1 * 6 + 0 ] = 1.0;
	A[ 2 * 6 + 0 ] = 2.0;
	A[ 0 * 6 + 1 ] = 1.0;
	A[ 1 * 6 + 1 ] = 3.0;
	A[ 2 * 6 + 1 ] = 1.0;
	A[ 0 * 6 + 2 ] = 2.0;
	A[ 1 * 6 + 2 ] = 1.0;
	A[ 2 * 6 + 2 ] = 5.0;
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 3 );
	info = dgerq2( 3, 3, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 3, 3 ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgerq2: 1x4 (single row)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = _1x4;
	A = new Float64Array( 6 * 4 );
	A[ 0 * 6 + 0 ] = 1.0;
	A[ 1 * 6 + 0 ] = 2.0;
	A[ 2 * 6 + 0 ] = 3.0;
	A[ 3 * 6 + 0 ] = 4.0;
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );
	info = dgerq2( 1, 4, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 1, 4 ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgerq2: 3x1 (single column)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = _3x1;
	A = new Float64Array( 6 * 1 );
	A[ 0 * 6 + 0 ] = 2.0;
	A[ 0 * 6 + 1 ] = 3.0;
	A[ 0 * 6 + 2 ] = 4.0;
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 3 );
	info = dgerq2( 3, 1, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 3, 1 ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgerq2: M=0 (quick return)', function t() {
	var WORK;
	var info;
	var TAU;
	var A;

	A = new Float64Array( 1 );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );
	info = dgerq2( 0, 3, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );
});

test( 'dgerq2: N=0 (quick return)', function t() {
	var WORK;
	var info;
	var TAU;
	var A;

	A = new Float64Array( 1 );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );
	info = dgerq2( 3, 0, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );
});

test( 'dgerq2: 1x1', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = _1x1;
	A = new Float64Array( 6 );
	A[ 0 ] = 7.0;
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );
	info = dgerq2( 1, 1, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 1, 1 ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgerq2: 2x5 (wide)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = _2x5;
	A = new Float64Array( 6 * 5 );
	A[ 0 * 6 + 0 ] = 1.0;
	A[ 1 * 6 + 0 ] = 2.0;
	A[ 2 * 6 + 0 ] = 3.0;
	A[ 3 * 6 + 0 ] = 4.0;
	A[ 4 * 6 + 0 ] = 5.0;
	A[ 0 * 6 + 1 ] = 6.0;
	A[ 1 * 6 + 1 ] = 7.0;
	A[ 2 * 6 + 1 ] = 8.0;
	A[ 3 * 6 + 1 ] = 9.0;
	A[ 4 * 6 + 1 ] = 10.0;
	TAU = new Float64Array( 2 );
	WORK = new Float64Array( 2 );
	info = dgerq2( 2, 5, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 2, 5 ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( TAU ), tc.TAU, 1e-14, 'TAU' );
});
