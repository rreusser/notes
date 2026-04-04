/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpbtrf = require( './../../dpbtrf/lib/base.js' );
var dpbtrs = require( './../lib/base.js' );

// FIXTURES //

var upper_tridiag_nrhs1 = require( './fixtures/upper_tridiag_nrhs1.json' );
var lower_tridiag_nrhs1 = require( './fixtures/lower_tridiag_nrhs1.json' );
var upper_tridiag_nrhs2 = require( './fixtures/upper_tridiag_nrhs2.json' );
var n_zero = require( './fixtures/n_zero.json' );
var nrhs_zero = require( './fixtures/nrhs_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var upper_penta_nrhs1 = require( './fixtures/upper_penta_nrhs1.json' );
var lower_penta_nrhs1 = require( './fixtures/lower_penta_nrhs1.json' );
var lower_penta_nrhs3 = require( './fixtures/lower_penta_nrhs3.json' );

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
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' ); // eslint-disable-line max-len
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

test( 'dpbtrs: main export is a function', function t() {
	assert.strictEqual( typeof dpbtrs, 'function' );
});

test( 'dpbtrs: upper tridiagonal N=5, NRHS=1', function t() {
	var info;
	var tc;
	var AB;
	var B;

	tc = upper_tridiag_nrhs1;
	AB = new Float64Array([
		0, 2, -1, 2, -1, 2, -1, 2, -1, 2
	]);
	dpbtrf( 'upper', 5, 1, AB, 1, 2, 0 );
	B = new Float64Array( [ 1, 0, 0, 0, 1 ] );
	info = dpbtrs( 'upper', 5, 1, 1, AB, 1, 2, 0, B, 1, 5, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbtrs: lower tridiagonal N=5, NRHS=1', function t() {
	var info;
	var tc;
	var AB;
	var B;

	tc = lower_tridiag_nrhs1;
	AB = new Float64Array([
		2, -1, 2, -1, 2, -1, 2, -1, 2, 0
	]);
	dpbtrf( 'lower', 5, 1, AB, 1, 2, 0 );
	B = new Float64Array( [ 1, 0, 0, 0, 1 ] );
	info = dpbtrs( 'lower', 5, 1, 1, AB, 1, 2, 0, B, 1, 5, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbtrs: upper tridiagonal N=5, NRHS=2', function t() {
	var info;
	var tc;
	var AB;
	var B;

	tc = upper_tridiag_nrhs2;
	AB = new Float64Array([
		0, 2, -1, 2, -1, 2, -1, 2, -1, 2
	]);
	dpbtrf( 'upper', 5, 1, AB, 1, 2, 0 );
	B = new Float64Array([
		1,
		2,
		3,
		4,
		5,
		5,
		4,
		3,
		2,
		1
	]);
	info = dpbtrs( 'upper', 5, 1, 2, AB, 1, 2, 0, B, 1, 5, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbtrs: N=0 quick return', function t() {
	var info;
	var tc;
	var AB;
	var B;

	tc = n_zero;
	AB = new Float64Array( 1 );
	B = new Float64Array( 1 );
	info = dpbtrs( 'upper', 0, 0, 1, AB, 1, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'dpbtrs: NRHS=0 quick return', function t() {
	var info;
	var tc;
	var AB;
	var B;

	tc = nrhs_zero;
	AB = new Float64Array( [ 2 ] );
	B = new Float64Array( 1 );
	info = dpbtrs( 'lower', 5, 1, 0, AB, 1, 2, 0, B, 1, 5, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'dpbtrs: N=1', function t() {
	var info;
	var tc;
	var AB;
	var B;

	tc = n_one;
	AB = new Float64Array( [ 2 ] );
	B = new Float64Array( [ 6 ] );
	info = dpbtrs( 'upper', 1, 0, 1, AB, 1, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbtrs: upper pentadiagonal N=4, NRHS=1', function t() {
	var info;
	var tc;
	var AB;
	var B;

	tc = upper_penta_nrhs1;
	AB = new Float64Array([
		0,
		0,
		6,
		0,
		-1,
		6,
		0.5,
		-1,
		6,
		0.5,
		-1,
		6
	]);
	dpbtrf( 'upper', 4, 2, AB, 1, 3, 0 );
	B = new Float64Array( [ 1, 2, 3, 4 ] );
	info = dpbtrs( 'upper', 4, 2, 1, AB, 1, 3, 0, B, 1, 4, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbtrs: lower pentadiagonal N=4, NRHS=1', function t() {
	var info;
	var tc;
	var AB;
	var B;

	tc = lower_penta_nrhs1;
	AB = new Float64Array([
		6,
		-1,
		0.5,
		6,
		-1,
		0.5,
		6,
		-1,
		0,
		6,
		0,
		0
	]);
	dpbtrf( 'lower', 4, 2, AB, 1, 3, 0 );
	B = new Float64Array( [ 1, 2, 3, 4 ] );
	info = dpbtrs( 'lower', 4, 2, 1, AB, 1, 3, 0, B, 1, 4, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-10, 'x' );
});

test( 'dpbtrs: lower pentadiagonal N=4, NRHS=3', function t() {
	var info;
	var tc;
	var AB;
	var B;

	tc = lower_penta_nrhs3;
	AB = new Float64Array([
		6,
		-1,
		0.5,
		6,
		-1,
		0.5,
		6,
		-1,
		0,
		6,
		0,
		0
	]);
	dpbtrf( 'lower', 4, 2, AB, 1, 3, 0 );
	B = new Float64Array([
		1,
		0,
		0,
		0,
		0,
		1,
		0,
		0,
		0,
		0,
		1,
		0
	]);
	info = dpbtrs( 'lower', 4, 2, 3, AB, 1, 3, 0, B, 1, 4, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( toArray( B ), tc.x, 1e-10, 'x' );
});
