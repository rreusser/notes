/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpttrf = require( './../lib/base.js' );

// FIXTURES //

var basic_5x5 = require( './fixtures/basic_5x5.json' );
var n_one = require( './fixtures/n_one.json' );
var n_zero = require( './fixtures/n_zero.json' );
var not_posdef_first = require( './fixtures/not_posdef_first.json' );
var not_posdef_mid = require( './fixtures/not_posdef_mid.json' );
var unrolled_8x8 = require( './fixtures/unrolled_8x8.json' );
var n_two = require( './fixtures/n_two.json' );

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

test( 'dpttrf: basic_5x5', function t() {
	var info;
	var tc;
	var d;
	var e;

	tc = basic_5x5;
	d = new Float64Array( [ 4.0, 4.0, 4.0, 4.0, 4.0 ] );
	e = new Float64Array( [ -1.0, -1.0, -1.0, -1.0 ] );
	info = dpttrf( 5, d, 1, 0, e, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
});

test( 'dpttrf: n_one', function t() {
	var info;
	var tc;
	var d;
	var e;

	tc = n_one;
	d = new Float64Array( [ 3.0 ] );
	e = new Float64Array( [] );
	info = dpttrf( 1, d, 1, 0, e, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dpttrf: n_zero', function t() {
	var info;
	var tc;
	var d;
	var e;

	tc = n_zero;
	d = new Float64Array( [] );
	e = new Float64Array( [] );
	info = dpttrf( 0, d, 1, 0, e, 1, 0 );
	assert.equal( info, tc.info, 'info' );
});

test( 'dpttrf: not_posdef_first', function t() {
	var info;
	var tc;
	var d;
	var e;

	tc = not_posdef_first;
	d = new Float64Array( [ -1.0, 4.0, 4.0 ] );
	e = new Float64Array( [ 1.0, 1.0 ] );
	info = dpttrf( 3, d, 1, 0, e, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
});

test( 'dpttrf: not_posdef_mid', function t() {
	var info;
	var tc;
	var d;
	var e;

	tc = not_posdef_mid;
	d = new Float64Array( [ 1.0, 1.0, 4.0 ] );
	e = new Float64Array( [ 2.0, 1.0 ] );
	info = dpttrf( 3, d, 1, 0, e, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
});

test( 'dpttrf: unrolled_8x8', function t() {
	var info;
	var tc;
	var d;
	var e;

	tc = unrolled_8x8;
	d = new Float64Array( [ 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0 ] );
	e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0 ] );
	info = dpttrf( 8, d, 1, 0, e, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
});

test( 'dpttrf: n_two', function t() {
	var info;
	var tc;
	var d;
	var e;

	tc = n_two;
	d = new Float64Array( [ 4.0, 4.0 ] );
	e = new Float64Array( [ 2.0 ] );
	info = dpttrf( 2, d, 1, 0, e, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
});

test( 'dpttrf: non-unit stride', function t() {
	var info;
	var tc;
	var d;
	var e;
	var i;

	tc = basic_5x5;
	d = new Float64Array( [ 0.0, 4.0, 0.0, 4.0, 0.0, 4.0, 0.0, 4.0, 0.0, 4.0 ] );
	e = new Float64Array( [ 0.0, -1.0, 0.0, -1.0, 0.0, -1.0, 0.0, -1.0 ] );
	info = dpttrf( 5, d, 2, 1, e, 2, 1 );
	assert.equal( info, tc.info, 'info' );
	for ( i = 0; i < 5; i++ ) {
		assertClose( d[ 1 + ( i * 2 ) ], tc.d[ i ], 1e-14, 'd[' + i + ']' );
	}
	for ( i = 0; i < 4; i++ ) {
		assertClose( e[ 1 + ( i * 2 ) ], tc.e[ i ], 1e-14, 'e[' + i + ']' );
	}
});

test( 'dpttrf: negative N returns 0', function t() {
	var info;
	var d;
	var e;

	d = new Float64Array( [ 1.0 ] );
	e = new Float64Array( [] );
	info = dpttrf( -1, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'negative N returns 0' );
});

test( 'dpttrf: fail at position 1 in unrolled loop (N=5)', function t() {
	var info;
	var d;
	var e;

	d = new Float64Array( [ 0.0, 4.0, 4.0, 4.0, 4.0 ] );
	e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	info = dpttrf( 5, d, 1, 0, e, 1, 0 );
	assert.equal( info, 1, 'info=1 at start of unrolled block' );
});

test( 'dpttrf: fail at position 2 in unrolled loop (N=5)', function t() {
	var info;
	var d;
	var e;

	d = new Float64Array( [ 1.0, 4.0, 4.0, 4.0, 4.0 ] );
	e = new Float64Array( [ 2.0, 1.0, 1.0, 1.0 ] );
	info = dpttrf( 5, d, 1, 0, e, 1, 0 );
	assert.equal( info, 2, 'info=2 at position i+1 in unrolled block' );
});

test( 'dpttrf: fail at position 3 in unrolled loop (N=5)', function t() {
	var info;
	var d;
	var e;

	d = new Float64Array( [ 1.0, 4.0, 1.0, 4.0, 4.0 ] );
	e = new Float64Array( [ 0.5, 4.0, 1.0, 1.0 ] );
	info = dpttrf( 5, d, 1, 0, e, 1, 0 );
	assert.equal( info, 3, 'info=3 at position i+2 in unrolled block' );
});

test( 'dpttrf: fail at position 4 in unrolled loop (N=5)', function t() {
	var info;
	var d;
	var e;

	d = new Float64Array( [ 4.0, 4.0, 4.0, 1.0, 4.0 ] );
	e = new Float64Array( [ 0.1, 0.1, 4.0, 1.0 ] );
	info = dpttrf( 5, d, 1, 0, e, 1, 0 );
	assert.equal( info, 4, 'info=4 at position i+3 in unrolled block' );
});

test( 'dpttrf: d(N) not positive definite (last element)', function t() {
	var info;
	var d;
	var e;

	d = new Float64Array( [ 4.0, 4.0, 4.0, 4.0, 1.0 ] );
	e = new Float64Array( [ 0.1, 0.1, 0.1, 4.0 ] );
	info = dpttrf( 5, d, 1, 0, e, 1, 0 );
	assert.equal( info, 5, 'info=N when d(N) <= 0' );
});
