/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsptrf = require( './../lib/base.js' );

// FIXTURES //

var _3x3_upper_spd = require( './fixtures/3x3_upper_spd.json' );
var _3x3_lower_spd = require( './fixtures/3x3_lower_spd.json' );
var _4x4_indef_upper = require( './fixtures/4x4_indef_upper.json' );
var _4x4_indef_lower = require( './fixtures/4x4_indef_lower.json' );
var n_one = require( './fixtures/n_one.json' );
var singular = require( './fixtures/singular.json' );
var _4x4_tridiag_lower = require( './fixtures/4x4_tridiag_lower.json' );
var _4x4_tridiag_upper = require( './fixtures/4x4_tridiag_upper.json' );
var n_one_singular = require( './fixtures/n_one_singular.json' );

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
* Converts Fortran 1-based IPIV to 0-based JS IPIV.
* Positive values: subtract 1. Negative values: unchanged (already encodes 0-based via ~kp).
*
* @private
* @param {Array} fipiv - Fortran IPIV array
* @returns {Array} JS IPIV array
*/
function convertIPIV( fipiv ) {
	var out = [];
	var i;
	for ( i = 0; i < fipiv.length; i++ ) {
		if ( fipiv[ i ] > 0 ) {
			out.push( fipiv[ i ] - 1 );
		} else {
			out.push( fipiv[ i ] );
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

test( 'dsptrf: 3x3_upper_spd', function t() {
	var ipiv;
	var info;
	var tc;
	var ap;

	tc = _3x3_upper_spd;
	ipiv = new Int32Array( 3 );
	ap = new Float64Array([
		4, 2, 5, 1, 3, 6
	]);
	info = dsptrf( 'upper', 3, ap, 1, 0, ipiv, 1, 0 );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( toArray( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsptrf: 3x3_lower_spd', function t() {
	var ipiv;
	var info;
	var tc;
	var ap;

	tc = _3x3_lower_spd;
	ipiv = new Int32Array( 3 );
	ap = new Float64Array([
		4, 2, 1, 5, 3, 6
	]);
	info = dsptrf( 'lower', 3, ap, 1, 0, ipiv, 1, 0 );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( toArray( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsptrf: 4x4_indef_upper', function t() {
	var ipiv;
	var info;
	var tc;
	var ap;

	tc = _4x4_indef_upper;
	ipiv = new Int32Array( 4 );
	ap = new Float64Array([
		0, 1, 0, 2, 4, 0, 3, 5, 6, 0
	]);
	info = dsptrf( 'upper', 4, ap, 1, 0, ipiv, 1, 0 );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( toArray( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsptrf: 4x4_indef_lower', function t() {
	var ipiv;
	var info;
	var tc;
	var ap;

	tc = _4x4_indef_lower;
	ipiv = new Int32Array( 4 );
	ap = new Float64Array([
		0, 1, 2, 3, 0, 4, 5, 0, 6, 0
	]);
	info = dsptrf( 'lower', 4, ap, 1, 0, ipiv, 1, 0 );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( toArray( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsptrf: n_zero', function t() {
	var ipiv;
	var info;
	var ap;

	ipiv = new Int32Array( 1 );
	ap = new Float64Array( 1 );
	info = dsptrf( 'lower', 0, ap, 1, 0, ipiv, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsptrf: n_one', function t() {
	var ipiv;
	var info;
	var tc;
	var ap;

	tc = n_one;
	ipiv = new Int32Array( 1 );
	ap = new Float64Array([ 5 ]);
	info = dsptrf( 'lower', 1, ap, 1, 0, ipiv, 1, 0 );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( toArray( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsptrf: singular', function t() {
	var ipiv;
	var info;
	var tc;
	var ap;

	tc = singular;
	ipiv = new Int32Array( 2 );
	ap = new Float64Array([ 0, 0, 0 ]);
	info = dsptrf( 'lower', 2, ap, 1, 0, ipiv, 1, 0 );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( toArray( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsptrf: 4x4_tridiag_lower', function t() {
	var ipiv;
	var info;
	var tc;
	var ap;

	tc = _4x4_tridiag_lower;
	ipiv = new Int32Array( 4 );
	ap = new Float64Array([
		2, -1, 0, 0, 2, -1, 0, 2, -1, 2
	]);
	info = dsptrf( 'lower', 4, ap, 1, 0, ipiv, 1, 0 );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( toArray( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsptrf: 4x4_tridiag_upper', function t() {
	var ipiv;
	var info;
	var tc;
	var ap;

	tc = _4x4_tridiag_upper;
	ipiv = new Int32Array( 4 );
	ap = new Float64Array([
		2, -1, 2, 0, -1, 2, 0, 0, -1, 2
	]);
	info = dsptrf( 'upper', 4, ap, 1, 0, ipiv, 1, 0 );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( toArray( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});

test( 'dsptrf: n_one_singular', function t() {
	var ipiv;
	var info;
	var tc;
	var ap;

	tc = n_one_singular;
	ipiv = new Int32Array( 1 );
	ap = new Float64Array([ 0 ]);
	info = dsptrf( 'upper', 1, ap, 1, 0, ipiv, 1, 0 );
	assertArrayClose( ap, tc.ap, 1e-14, 'ap' );
	assert.equal( info, tc.info, 'info' );
	assert.deepEqual( toArray( ipiv ), convertIPIV( tc.ipiv ), 'ipiv' );
});
