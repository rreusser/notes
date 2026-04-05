/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dtptri = require( './../lib/base.js' );

// FIXTURES //

var upper_nonunit_3x3 = require( './fixtures/upper_nonunit_3x3.json' );
var lower_nonunit_3x3 = require( './fixtures/lower_nonunit_3x3.json' );
var upper_unit_3x3 = require( './fixtures/upper_unit_3x3.json' );
var lower_unit_3x3 = require( './fixtures/lower_unit_3x3.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var upper_nonunit_4x4 = require( './fixtures/upper_nonunit_4x4.json' );
var lower_nonunit_4x4 = require( './fixtures/lower_nonunit_4x4.json' );
var singular_upper = require( './fixtures/singular_upper.json' );
var singular_lower = require( './fixtures/singular_lower.json' );
var singular_lower_last = require( './fixtures/singular_lower_last.json' );
var singular_upper_first = require( './fixtures/singular_upper_first.json' );
var identity_upper = require( './fixtures/identity_upper.json' );

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

test( 'dtptri: upper_nonunit_3x3', function t() {
	var info;
	var tc;
	var ap;

	tc = upper_nonunit_3x3;
	ap = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
	info = dtptri( 'upper', 'non-unit', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dtptri: lower_nonunit_3x3', function t() {
	var info;
	var tc;
	var ap;

	tc = lower_nonunit_3x3;
	ap = new Float64Array( [ 2.0, 1.0, 3.0, 4.0, 5.0, 6.0 ] );
	info = dtptri( 'lower', 'non-unit', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dtptri: upper_unit_3x3', function t() {
	var info;
	var tc;
	var ap;

	tc = upper_unit_3x3;
	ap = new Float64Array( [ 99.0, 1.0, 99.0, 3.0, 5.0, 99.0 ] );
	info = dtptri( 'upper', 'unit', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dtptri: lower_unit_3x3', function t() {
	var info;
	var tc;
	var ap;

	tc = lower_unit_3x3;
	ap = new Float64Array( [ 99.0, 1.0, 3.0, 99.0, 5.0, 99.0 ] );
	info = dtptri( 'lower', 'unit', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dtptri: n_zero', function t() {
	var info;
	var tc;
	var ap;

	tc = n_zero;
	ap = new Float64Array( [ 1.0 ] );
	info = dtptri( 'upper', 'non-unit', 0, ap, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dtptri: n_one', function t() {
	var info;
	var tc;
	var ap;

	tc = n_one;
	ap = new Float64Array( [ 4.0 ] );
	info = dtptri( 'upper', 'non-unit', 1, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dtptri: upper_nonunit_4x4', function t() {
	var info;
	var tc;
	var ap;

	tc = upper_nonunit_4x4;
	ap = new Float64Array( [ 2.0, 1.0, 4.0, 3.0, 5.0, 6.0, 7.0, 2.0, 1.0, 3.0 ] );
	info = dtptri( 'upper', 'non-unit', 4, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dtptri: lower_nonunit_4x4', function t() {
	var info;
	var tc;
	var ap;

	tc = lower_nonunit_4x4;
	ap = new Float64Array( [ 3.0, 1.0, 4.0, 2.0, 2.0, 1.0, 3.0, 5.0, 1.0, 4.0 ] );
	info = dtptri( 'lower', 'non-unit', 4, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dtptri: singular_upper', function t() {
	var info;
	var tc;
	var ap;

	tc = singular_upper;
	ap = new Float64Array( [ 2.0, 1.0, 0.0, 3.0, 5.0, 6.0 ] );
	info = dtptri( 'upper', 'non-unit', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dtptri: singular_lower', function t() {
	var info;
	var tc;
	var ap;

	tc = singular_lower;
	ap = new Float64Array( [ 0.0, 1.0, 3.0, 4.0, 5.0, 6.0 ] );
	info = dtptri( 'lower', 'non-unit', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dtptri: singular_lower_last', function t() {
	var info;
	var tc;
	var ap;

	tc = singular_lower_last;
	ap = new Float64Array( [ 2.0, 1.0, 3.0, 4.0, 5.0, 0.0 ] );
	info = dtptri( 'lower', 'non-unit', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dtptri: singular_upper_first', function t() {
	var info;
	var tc;
	var ap;

	tc = singular_upper_first;
	ap = new Float64Array( [ 0.0, 1.0, 4.0, 3.0, 5.0, 6.0 ] );
	info = dtptri( 'upper', 'non-unit', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dtptri: identity_upper', function t() {
	var info;
	var tc;
	var ap;

	tc = identity_upper;
	ap = new Float64Array( [ 1.0, 0.0, 1.0, 0.0, 0.0, 1.0 ] );
	info = dtptri( 'upper', 'non-unit', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
});

test( 'dtptri: supports stride and offset', function t() {
	var result;
	var info;
	var ap;
	var tc;
	var i;

	ap = new Float64Array( [ 0.0, 0.0, 2.0, 0.0, 1.0, 0.0, 4.0, 0.0, 3.0, 0.0, 5.0, 0.0, 6.0, 0.0 ] ); // eslint-disable-line max-len
	tc = upper_nonunit_3x3;
	info = dtptri( 'upper', 'non-unit', 3, ap, 2, 2 );
	result = new Float64Array( 6 );
	assert.equal( info, tc.info );
	for ( i = 0; i < 6; i++ ) {
		result[ i ] = ap[ 2 + ( i * 2 ) ];
	}
	assertArrayClose( result, tc.ap, 1e-14, 'ap with stride/offset' );
});
