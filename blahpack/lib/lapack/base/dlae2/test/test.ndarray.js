/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlae2 = require( './../lib/base.js' );

// FIXTURES //

var diagonal_a_gt_c = require( './fixtures/diagonal_a_gt_c.json' );
var diagonal_c_gt_a = require( './fixtures/diagonal_c_gt_a.json' );
var off_diagonal = require( './fixtures/off_diagonal.json' );
var equal_diagonal = require( './fixtures/equal_diagonal.json' );
var general = require( './fixtures/general.json' );
var negative_diagonal = require( './fixtures/negative_diagonal.json' );
var sm_zero = require( './fixtures/sm_zero.json' );
var identity = require( './fixtures/identity.json' );
var adf_lt_ab = require( './fixtures/adf_lt_ab.json' );
var adf_eq_ab = require( './fixtures/adf_eq_ab.json' );

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
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
}

// TESTS //

test( 'dlae2 is a function', function t() {
	assert.strictEqual( typeof dlae2, 'function' );
} );

test( 'dlae2 returns an object with rt1 and rt2', function t() {
	var out = dlae2( 1.0, 0.0, 1.0 );
	assert.strictEqual( typeof out, 'object' );
	assert.strictEqual( typeof out.rt1, 'number' );
	assert.strictEqual( typeof out.rt2, 'number' );
} );

test( 'dlae2: diagonal matrix (a > c)', function t() {
	var out;
	var tc;

	tc = diagonal_a_gt_c;
	out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
} );

test( 'dlae2: diagonal matrix (c > a)', function t() {
	var out;
	var tc;

	tc = diagonal_c_gt_a;
	out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
} );

test( 'dlae2: off-diagonal only', function t() {
	var out;
	var tc;

	tc = off_diagonal;
	out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
} );

test( 'dlae2: equal diagonal', function t() {
	var out;
	var tc;

	tc = equal_diagonal;
	out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
} );

test( 'dlae2: general 2x2', function t() {
	var out;
	var tc;

	tc = general;
	out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
} );

test( 'dlae2: negative diagonal (sm < 0)', function t() {
	var out;
	var tc;

	tc = negative_diagonal;
	out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
} );

test( 'dlae2: sm = 0 (a = -c)', function t() {
	var out;
	var tc;

	tc = sm_zero;
	out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
} );

test( 'dlae2: identity matrix', function t() {
	var out;
	var tc;

	tc = identity;
	out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
} );

test( 'dlae2: adf < ab path', function t() {
	var out;
	var tc;

	tc = adf_lt_ab;
	out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
} );

test( 'dlae2: adf = ab path', function t() {
	var out;
	var tc;

	tc = adf_eq_ab;
	out = dlae2( tc.a, tc.b, tc.c );
	assertClose( out.rt1, tc.rt1, 1e-14, 'rt1' );
	assertClose( out.rt2, tc.rt2, 1e-14, 'rt2' );
} );

test( 'dlae2: zero matrix (all zeros)', function t() {
	var out = dlae2( 0.0, 0.0, 0.0 );
	assert.strictEqual( out.rt1, 0.0 );
	assert.ok( Math.abs( out.rt2 ) === 0.0, 'rt2 is zero' );
} );

test( 'dlae2: eigenvalue property (rt1*rt2 = det, rt1+rt2 = trace)', function t() {
	var trace;
	var out;
	var det;
	var a;
	var b;
	var c;

	a = 3.0;
	b = 1.5;
	c = 2.0;
	out = dlae2( a, b, c );
	trace = a + c;
	det = ( a * c ) - ( b * b );

	// Sum of eigenvalues = trace
	assertClose( out.rt1 + out.rt2, trace, 1e-14, 'trace' );

	// Product of eigenvalues = determinant
	assertClose( out.rt1 * out.rt2, det, 1e-14, 'det' );
} );
