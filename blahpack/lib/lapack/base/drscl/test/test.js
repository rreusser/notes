/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var drscl = require( './../lib/base.js' );

// FIXTURES //

var basic = require( './fixtures/basic.json' );
var half = require( './fixtures/half.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var stride2 = require( './fixtures/stride2.json' );
var identity = require( './fixtures/identity.json' );
var large_sa = require( './fixtures/large_sa.json' );
var small_sa = require( './fixtures/small_sa.json' );
var very_large_sa = require( './fixtures/very_large_sa.json' );
var very_small_sa = require( './fixtures/very_small_sa.json' );
var negative_sa = require( './fixtures/negative_sa.json' );

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

test( 'drscl: basic', function t() {
	var tc = basic;
	var x = new Float64Array( [ 2.0, 4.0, 6.0, 8.0 ] );
	drscl( 4, 2.0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: half', function t() {
	var tc = half;
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	drscl( 3, 0.5, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: n_zero', function t() {
	var tc = n_zero;
	var x = new Float64Array( [ 99.0 ] );
	drscl( 0, 2.0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: n_one', function t() {
	var tc = n_one;
	var x = new Float64Array( [ 10.0 ] );
	drscl( 1, 5.0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: stride2', function t() {
	var tc = stride2;
	var x = new Float64Array( [ 10.0, 99.0, 20.0, 99.0, 30.0 ] );
	drscl( 3, 10.0, x, 2, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: identity', function t() {
	var tc = identity;
	var x = new Float64Array( [ 3.0, 7.0 ] );
	drscl( 2, 1.0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: large_sa', function t() {
	var tc = large_sa;
	var x = new Float64Array( [ 1.0, 2.0 ] );
	drscl( 2, 1e300, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: small_sa', function t() {
	var tc = small_sa;
	var x = new Float64Array( [ 1e-300, 2e-300 ] );
	drscl( 2, 1e-300, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: very_large_sa (triggers SMLNUM branch)', function t() {
	var tc = very_large_sa;
	var x = new Float64Array( [ 1.0, 2.0 ] );
	drscl( 2, 1e308, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: very_small_sa (triggers BIGNUM branch)', function t() {
	var tc = very_small_sa;
	var x = new Float64Array( [ 1e-308, 2e-308 ] );
	drscl( 2, 1e-309, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: negative_sa', function t() {
	var tc = negative_sa;
	var x = new Float64Array( [ 6.0, -9.0, 12.0 ] );
	drscl( 3, -3.0, x, 1, 0 );
	assertArrayClose( x, tc.x, 1e-14, 'x' );
});

test( 'drscl: offset', function t() {
	// Test with non-zero offset
	var x = new Float64Array( [ 999.0, 10.0, 20.0, 30.0 ] );
	drscl( 3, 10.0, x, 1, 1 );
	assert.equal( x[ 0 ], 999.0 ); // untouched
	assertClose( x[ 1 ], 1.0, 1e-14, 'x[1]' );
	assertClose( x[ 2 ], 2.0, 1e-14, 'x[2]' );
	assertClose( x[ 3 ], 3.0, 1e-14, 'x[3]' );
});
