/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpptrf = require( './../lib/base.js' );

// FIXTURES //

var upper_3x3 = require( './fixtures/upper_3x3.json' );
var lower_3x3 = require( './fixtures/lower_3x3.json' );
var upper_4x4 = require( './fixtures/upper_4x4.json' );
var lower_4x4 = require( './fixtures/lower_4x4.json' );
var n_one_upper = require( './fixtures/n_one_upper.json' );
var n_one_lower = require( './fixtures/n_one_lower.json' );
var not_hpd_upper = require( './fixtures/not_hpd_upper.json' );
var not_hpd_lower = require( './fixtures/not_hpd_lower.json' );
var identity_upper = require( './fixtures/identity_upper.json' );
var identity_lower = require( './fixtures/identity_lower.json' );
var upper_2x2 = require( './fixtures/upper_2x2.json' );
var lower_2x2 = require( './fixtures/lower_2x2.json' );

// FUNCTIONS //

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
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1e-14 ); // eslint-disable-line max-len
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
	}
}

/**
* Creates a Complex128Array from interleaved re/im values.
*
* @private
* @param {Array<number>} arr - interleaved values
* @returns {Complex128Array} complex array
*/
function c128( arr ) {
	return new Complex128Array( new Float64Array( arr ) );
}

// TESTS //

test( 'zpptrf is a function', function t() {
	assert.equal( typeof zpptrf, 'function' );
});

test( 'zpptrf: upper_3x3 (N=3, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = upper_3x3;
	ap = c128( [ 10, 0, 2, 1, 8, 0, 3, -2, 1, 1, 6, 0 ] );
	info = zpptrf( 'upper', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.ap, 1e-14, 'ap' );
});

test( 'zpptrf: lower_3x3 (N=3, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = lower_3x3;
	ap = c128( [ 10, 0, 2, -1, 3, 2, 8, 0, 1, -1, 6, 0 ] );
	info = zpptrf( 'lower', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.ap, 1e-14, 'ap' );
});

test( 'zpptrf: upper_4x4 (N=4, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = upper_4x4;
	ap = c128([
		20,
		0,
		3,
		1,
		15,
		0,
		1,
		-2,
		2,
		3,
		12,
		0,
		4,
		1,
		1,
		-1,
		3,
		2,
		10,
		0
	]);
	info = zpptrf( 'upper', 4, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.ap, 1e-14, 'ap' );
});

test( 'zpptrf: lower_4x4 (N=4, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = lower_4x4;
	ap = c128([
		20,
		0,
		3,
		-1,
		1,
		2,
		4,
		-1,
		15,
		0,
		2,
		-3,
		1,
		1,
		12,
		0,
		3,
		-2,
		10,
		0
	]);
	info = zpptrf( 'lower', 4, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.ap, 1e-14, 'ap' );
});

test( 'zpptrf: n_zero (N=0, quick return)', function t() {
	var info;
	var ap;

	ap = c128( [ 1, 0 ] );
	info = zpptrf( 'upper', 0, ap, 1, 0 );
	assert.equal( info, 0 );
});

test( 'zpptrf: n_one_upper (N=1, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = n_one_upper;
	ap = c128( [ 9, 0 ] );
	info = zpptrf( 'upper', 1, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.ap, 1e-14, 'ap' );
});

test( 'zpptrf: n_one_lower (N=1, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = n_one_lower;
	ap = c128( [ 16, 0 ] );
	info = zpptrf( 'lower', 1, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.ap, 1e-14, 'ap' );
});

test( 'zpptrf: not_hpd_upper (info > 0, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = not_hpd_upper;
	ap = c128( [ 1, 0, 2, 1, 1, 0 ] );
	info = zpptrf( 'upper', 2, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.ap, 1e-14, 'ap' );
});

test( 'zpptrf: not_hpd_lower (info > 0, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = not_hpd_lower;
	ap = c128( [ 1, 0, 2, -1, 1, 0 ] );
	info = zpptrf( 'lower', 2, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.ap, 1e-14, 'ap' );
});

test( 'zpptrf: not_hpd_first_upper (info=1 at first diagonal, uplo=upper)', function t() { // eslint-disable-line max-len
	var info;
	var ap;

	ap = c128( [ -4, 0, 1, 0, 5, 0 ] );
	info = zpptrf( 'upper', 2, ap, 1, 0 );
	assert.equal( info, 1 );
});

test( 'zpptrf: not_hpd_first_lower (info=1 at first diagonal, uplo=lower)', function t() { // eslint-disable-line max-len
	var info;
	var ap;

	ap = c128( [ 0, 0, 1, 0, 5, 0 ] );
	info = zpptrf( 'lower', 2, ap, 1, 0 );
	assert.equal( info, 1 );
});

test( 'zpptrf: identity_upper (N=3, identity, uplo=upper)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var ap;

	tc = identity_upper;
	ap = c128( [ 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0 ] );
	info = zpptrf( 'upper', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.ap, 1e-14, 'ap' );
});

test( 'zpptrf: identity_lower (N=3, identity, uplo=lower)', function t() { // eslint-disable-line max-len
	var info;
	var tc;
	var ap;

	tc = identity_lower;
	ap = c128( [ 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0 ] );
	info = zpptrf( 'lower', 3, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.ap, 1e-14, 'ap' );
});

test( 'zpptrf: upper_2x2 (N=2, uplo=upper)', function t() {
	var info;
	var tc;
	var ap;

	tc = upper_2x2;
	ap = c128( [ 4, 0, 1, 2, 10, 0 ] );
	info = zpptrf( 'upper', 2, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.ap, 1e-14, 'ap' );
});

test( 'zpptrf: lower_2x2 (N=2, uplo=lower)', function t() {
	var info;
	var tc;
	var ap;

	tc = lower_2x2;
	ap = c128( [ 4, 0, 1, -2, 10, 0 ] );
	info = zpptrf( 'lower', 2, ap, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( reinterpret( ap, 0 ), tc.ap, 1e-14, 'ap' );
});

test( 'zpptrf: supports non-unit stride (upper)', function t() {
	var info;
	var tc;
	var ap;
	var v;

	tc = upper_2x2;
	ap = c128([
		999,
		999,
		4,
		0,
		999,
		999,
		1,
		2,
		999,
		999,
		10,
		0,
		999,
		999
	]);
	info = zpptrf( 'upper', 2, ap, 2, 1 );
	v = reinterpret( ap, 0 );
	assert.equal( info, tc.info );
	assert.equal( v[ 2 ], tc.ap[ 0 ] );
	assert.equal( v[ 3 ], tc.ap[ 1 ] );
	assert.equal( v[ 6 ], tc.ap[ 2 ] );
	assert.equal( v[ 7 ], tc.ap[ 3 ] );
	assert.equal( v[ 10 ], tc.ap[ 4 ] );
	assert.equal( v[ 11 ], tc.ap[ 5 ] );
});
