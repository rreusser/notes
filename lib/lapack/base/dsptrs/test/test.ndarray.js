/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsptrf = require( '../../dsptrf/lib/base.js' );
var dsptrs = require( './../lib/ndarray.js' );

// FIXTURES //

var _3x3_upper_1rhs = require( './fixtures/3x3_upper_1rhs.json' );
var _3x3_lower_1rhs = require( './fixtures/3x3_lower_1rhs.json' );
var _3x3_lower_2rhs = require( './fixtures/3x3_lower_2rhs.json' );
var _4x4_indef_lower_1rhs = require( './fixtures/4x4_indef_lower_1rhs.json' );
var _4x4_indef_upper_1rhs = require( './fixtures/4x4_indef_upper_1rhs.json' );
var n_one_lower = require( './fixtures/n_one_lower.json' );
var n_one_upper = require( './fixtures/n_one_upper.json' );
var _4x4_indef_upper_2rhs = require( './fixtures/4x4_indef_upper_2rhs.json' );
var _3x3_upper_2rhs = require( './fixtures/3x3_upper_2rhs.json' );

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

test( 'dsptrs: 3x3_upper_1rhs', function t() {
	var IPIV;
	var info;
	var tc;
	var AP;
	var B;

	tc = _3x3_upper_1rhs;
	AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 7.0, 10.0, 10.0 ] );
	dsptrf( 'upper', 3, AP, 1, 0, IPIV, 1, 0 );
	info = dsptrs( 'upper', 3, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 3, 0 );
	assertArrayClose( B, tc.b, 1e-14, 'b' );
	assert.equal( info, tc.info );
});

test( 'dsptrs: 3x3_lower_1rhs', function t() {
	var IPIV;
	var info;
	var tc;
	var AP;
	var B;

	tc = _3x3_lower_1rhs;
	AP = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 7.0, 10.0, 10.0 ] );
	dsptrf( 'lower', 3, AP, 1, 0, IPIV, 1, 0 );
	info = dsptrs( 'lower', 3, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 3, 0 );
	assertArrayClose( B, tc.b, 1e-14, 'b' );
	assert.equal( info, tc.info );
});

test( 'dsptrs: 3x3_lower_2rhs', function t() {
	var IPIV;
	var info;
	var tc;
	var AP;
	var B;

	tc = _3x3_lower_2rhs;
	AP = new Float64Array( [ 4.0, 2.0, 1.0, 5.0, 3.0, 6.0 ] );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 7.0, 10.0, 10.0, 18.0, 31.0, 35.0 ] );
	dsptrf( 'lower', 3, AP, 1, 0, IPIV, 1, 0 );
	info = dsptrs( 'lower', 3, 2, AP, 1, 0, IPIV, 1, 0, B, 1, 3, 0 );
	assertArrayClose( B, tc.b, 1e-14, 'b' );
	assert.equal( info, tc.info );
});

test( 'dsptrs: 4x4_indef_lower_1rhs', function t() {
	var IPIV;
	var info;
	var tc;
	var AP;
	var B;

	tc = _4x4_indef_lower_1rhs;
	AP = new Float64Array( [ 0.0, 1.0, 2.0, 3.0, 0.0, 4.0, 5.0, 0.0, 6.0, 0.0 ] );
	IPIV = new Int32Array( 4 );
	B = new Float64Array( [ 6.0, 10.0, 12.0, 14.0 ] );
	dsptrf( 'lower', 4, AP, 1, 0, IPIV, 1, 0 );
	info = dsptrs( 'lower', 4, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 4, 0 );
	assertArrayClose( B, tc.b, 1e-14, 'b' );
	assert.equal( info, tc.info );
});

test( 'dsptrs: 4x4_indef_upper_1rhs', function t() {
	var IPIV;
	var info;
	var tc;
	var AP;
	var B;

	tc = _4x4_indef_upper_1rhs;
	AP = new Float64Array( [ 0.0, 1.0, 0.0, 2.0, 4.0, 0.0, 3.0, 5.0, 6.0, 0.0 ] );
	IPIV = new Int32Array( 4 );
	B = new Float64Array( [ 6.0, 10.0, 12.0, 14.0 ] );
	dsptrf( 'upper', 4, AP, 1, 0, IPIV, 1, 0 );
	info = dsptrs( 'upper', 4, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 4, 0 );
	assertArrayClose( B, tc.b, 1e-14, 'b' );
	assert.equal( info, tc.info );
});

test( 'dsptrs: n_zero', function t() {
	var IPIV;
	var info;
	var AP;
	var B;

	AP = new Float64Array( 1 );
	IPIV = new Int32Array( 1 );
	B = new Float64Array( 1 );
	info = dsptrs( 'lower', 0, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assert.equal( info, 0 );
});

test( 'dsptrs: nrhs_zero', function t() {
	var IPIV;
	var info;
	var AP;
	var B;

	AP = new Float64Array( 6 );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( 3 );
	info = dsptrs( 'lower', 3, 0, AP, 1, 0, IPIV, 1, 0, B, 1, 3, 0 );
	assert.equal( info, 0 );
});

test( 'dsptrs: n_one_lower', function t() {
	var IPIV;
	var info;
	var tc;
	var AP;
	var B;

	tc = n_one_lower;
	AP = new Float64Array( [ 4.0 ] );
	IPIV = new Int32Array( 1 );
	B = new Float64Array( [ 8.0 ] );
	dsptrf( 'lower', 1, AP, 1, 0, IPIV, 1, 0 );
	info = dsptrs( 'lower', 1, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assertArrayClose( B, tc.b, 1e-14, 'b' );
	assert.equal( info, tc.info );
});

test( 'dsptrs: n_one_upper', function t() {
	var IPIV;
	var info;
	var tc;
	var AP;
	var B;

	tc = n_one_upper;
	AP = new Float64Array( [ 5.0 ] );
	IPIV = new Int32Array( 1 );
	B = new Float64Array( [ 15.0 ] );
	dsptrf( 'upper', 1, AP, 1, 0, IPIV, 1, 0 );
	info = dsptrs( 'upper', 1, 1, AP, 1, 0, IPIV, 1, 0, B, 1, 1, 0 );
	assertArrayClose( B, tc.b, 1e-14, 'b' );
	assert.equal( info, tc.info );
});

test( 'dsptrs: 4x4_indef_upper_2rhs', function t() {
	var IPIV;
	var info;
	var tc;
	var AP;
	var B;

	tc = _4x4_indef_upper_2rhs;
	AP = new Float64Array( [ 0.0, 1.0, 0.0, 2.0, 4.0, 0.0, 3.0, 5.0, 6.0, 0.0 ] );
	IPIV = new Int32Array( 4 );
	B = new Float64Array( [ 6.0, 10.0, 12.0, 14.0, 20.0, 34.0, 40.0, 44.0 ] );
	dsptrf( 'upper', 4, AP, 1, 0, IPIV, 1, 0 );
	info = dsptrs( 'upper', 4, 2, AP, 1, 0, IPIV, 1, 0, B, 1, 4, 0 );
	assertArrayClose( B, tc.b, 1e-14, 'b' );
	assert.equal( info, tc.info );
});

test( 'dsptrs: 3x3_upper_2rhs', function t() {
	var IPIV;
	var info;
	var tc;
	var AP;
	var B;

	tc = _3x3_upper_2rhs;
	AP = new Float64Array( [ 4.0, 2.0, 5.0, 1.0, 3.0, 6.0 ] );
	IPIV = new Int32Array( 3 );
	B = new Float64Array( [ 7.0, 10.0, 10.0, 18.0, 31.0, 35.0 ] );
	dsptrf( 'upper', 3, AP, 1, 0, IPIV, 1, 0 );
	info = dsptrs( 'upper', 3, 2, AP, 1, 0, IPIV, 1, 0, B, 1, 3, 0 );
	assertArrayClose( B, tc.b, 1e-14, 'b' );
	assert.equal( info, tc.info );
});
