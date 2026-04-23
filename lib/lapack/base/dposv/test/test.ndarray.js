/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dposv = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );

// FIXTURES //

var lower_3x3 = require( './fixtures/lower_3x3.json' );
var upper_3x3 = require( './fixtures/upper_3x3.json' );
var not_posdef = require( './fixtures/not_posdef.json' );
var n_zero = require( './fixtures/n_zero.json' );
var identity = require( './fixtures/identity.json' );
var multi_rhs = require( './fixtures/multi_rhs.json' );
var nrhs_zero = require( './fixtures/nrhs_zero.json' );

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

test( 'dposv: lower_3x3', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = lower_3x3;
	A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	B = new Float64Array( [ 1, 2, 3 ] );
	info = dposv( 'lower', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});

test( 'dposv: upper_3x3', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = upper_3x3;
	A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	B = new Float64Array( [ 1, 2, 3 ] );
	info = dposv( 'upper', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});

test( 'dposv: not_posdef', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = not_posdef;
	A = new Float64Array( [ 1, 2, 3, 2, 1, 4, 3, 4, 1 ] );
	B = new Float64Array( [ 1, 1, 1 ] );
	info = dposv( 'lower', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'dposv: n_zero', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = n_zero;
	A = new Float64Array( 1 );
	B = new Float64Array( 1 );
	info = dposv( 'lower', 0, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dposv: identity', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = identity;
	A = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	B = new Float64Array( [ 3, 5, 7 ] );
	info = dposv( 'lower', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});

test( 'dposv: multi_rhs', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = multi_rhs;
	A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	B = new Float64Array( [ 1, 0, 0, 0, 1, 0 ] );
	info = dposv( 'lower', 3, 2, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-14, 'x' );
});

test( 'dposv: nrhs_zero', function t() {
	var info;
	var tc;
	var A;
	var B;

	tc = nrhs_zero;
	A = new Float64Array( 9 );
	B = new Float64Array( 3 );
	info = dposv( 'lower', 3, 0, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

// ndarray validation tests

test( 'dposv: ndarray throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		ndarray( 'invalid', 3, 1, new Float64Array( 9 ), 1, 3, 0, new Float64Array( 3 ), 1, 3, 0 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'dposv: ndarray throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ndarray( 'upper', -1, 1, new Float64Array( 9 ), 1, 3, 0, new Float64Array( 3 ), 1, 3, 0 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'dposv: ndarray throws RangeError for negative NRHS', function t() {
	assert.throws( function throws() {
		ndarray( 'upper', 3, -1, new Float64Array( 9 ), 1, 3, 0, new Float64Array( 3 ), 1, 3, 0 ); // eslint-disable-line max-len
	}, RangeError );
});
