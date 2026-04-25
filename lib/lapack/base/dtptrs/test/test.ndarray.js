/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtptrs = require( './../lib/ndarray.js' );

// FIXTURES //

var upper_no_trans = require( './fixtures/upper_no_trans.json' );
var lower_no_trans = require( './fixtures/lower_no_trans.json' );
var upper_trans = require( './fixtures/upper_trans.json' );
var lower_trans = require( './fixtures/lower_trans.json' );
var upper_unit_diag = require( './fixtures/upper_unit_diag.json' );
var lower_unit_diag = require( './fixtures/lower_unit_diag.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var singular_upper = require( './fixtures/singular_upper.json' );
var singular_lower = require( './fixtures/singular_lower.json' );
var singular_lower_last = require( './fixtures/singular_lower_last.json' );
var multi_rhs = require( './fixtures/multi_rhs.json' );
var upper_conj_trans = require( './fixtures/upper_conj_trans.json' );
var lower_4x4 = require( './fixtures/lower_4x4.json' );

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

test( 'dtptrs is a function', function t() {
	assert.equal( typeof dtptrs, 'function' );
});

test( 'dtptrs: upper_no_trans', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = upper_no_trans;
	ap = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'upper', 'no-transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: lower_no_trans', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = lower_no_trans;
	ap = new Float64Array( [ 2.0, 1.0, 3.0, 4.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'lower', 'no-transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: upper_trans', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = upper_trans;
	ap = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'upper', 'transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: lower_trans', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = lower_trans;
	ap = new Float64Array( [ 2.0, 1.0, 3.0, 4.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'lower', 'transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: upper_unit_diag', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = upper_unit_diag;
	ap = new Float64Array( [ 1.0, 2.0, 1.0, 3.0, 4.0, 1.0 ] );
	b = new Float64Array( [ 10.0, 5.0, 1.0 ] );
	info = dtptrs( 'upper', 'no-transpose', 'unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: lower_unit_diag', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = lower_unit_diag;
	ap = new Float64Array( [ 1.0, 2.0, 3.0, 1.0, 4.0, 1.0 ] );
	b = new Float64Array( [ 10.0, 5.0, 1.0 ] );
	info = dtptrs( 'lower', 'no-transpose', 'unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: n_zero (quick return)', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = n_zero;
	ap = new Float64Array( [ 1.0 ] );
	b = new Float64Array( [ 99.0 ] );
	info = dtptrs( 'upper', 'no-transpose', 'non-unit', 0, 1, ap, 1, 0, b, 1, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
});

test( 'dtptrs: n_one', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = n_one;
	ap = new Float64Array( [ 5.0 ] );
	b = new Float64Array( [ 15.0 ] );
	info = dtptrs( 'upper', 'no-transpose', 'non-unit', 1, 1, ap, 1, 0, b, 1, 1, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: singular_upper (info > 0)', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = singular_upper;
	ap = new Float64Array( [ 2.0, 1.0, 0.0, 3.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'upper', 'no-transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
});

test( 'dtptrs: singular_lower (info > 0)', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = singular_lower;
	ap = new Float64Array( [ 0.0, 1.0, 3.0, 4.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'lower', 'no-transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
});

test( 'dtptrs: singular_lower_last (info > 0)', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = singular_lower_last;
	ap = new Float64Array( [ 2.0, 1.0, 3.0, 4.0, 5.0, 0.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'lower', 'no-transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
});

test( 'dtptrs: multi_rhs', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = multi_rhs;
	ap = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	info = dtptrs( 'upper', 'no-transpose', 'non-unit', 3, 2, ap, 1, 0, b, 1, 3, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: upper_conj_trans (same as transpose for real)', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = upper_conj_trans;
	ap = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
	b = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	info = dtptrs( 'upper', 'transpose', 'non-unit', 3, 1, ap, 1, 0, b, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});

test( 'dtptrs: lower_4x4', function t() {
	var info;
	var tc;
	var ap;
	var b;

	tc = lower_4x4;
	ap = new Float64Array( [ 3.0, 1.0, 4.0, 2.0, 2.0, 1.0, 3.0, 5.0, 1.0, 4.0 ] );
	b = new Float64Array( [ 10.0, 20.0, 30.0, 40.0 ] );
	info = dtptrs( 'lower', 'no-transpose', 'non-unit', 4, 1, ap, 1, 0, b, 1, 4, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info );
	assertArrayClose( b, tc.x, 1e-14, 'x' );
});
