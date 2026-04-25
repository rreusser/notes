/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dpbtf2 = require( './../lib/ndarray.js' );

// FIXTURES //

var upper_tridiag_5 = require( './fixtures/upper_tridiag_5.json' );
var lower_tridiag_5 = require( './fixtures/lower_tridiag_5.json' );
var upper_penta_4 = require( './fixtures/upper_penta_4.json' );
var lower_penta_4 = require( './fixtures/lower_penta_4.json' );
var n_one = require( './fixtures/n_one.json' );
var n_zero = require( './fixtures/n_zero.json' );
var not_posdef = require( './fixtures/not_posdef.json' );
var upper_full_3 = require( './fixtures/upper_full_3.json' );

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

test( 'dpbtf2: upper_tridiag_5', function t() {
	var info;
	var tc;
	var ab;

	tc = upper_tridiag_5;
	ab = new Float64Array([
		0.0,
		2.0,   // col 0
		-1.0,
		2.0,   // col 1
		-1.0,
		2.0,   // col 2
		-1.0,
		2.0,   // col 3
		-1.0,
		2.0    // col 4
	]);
	info = dpbtf2( 'upper', 5, 1, ab, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: lower_tridiag_5', function t() {
	var info;
	var tc;
	var ab;

	tc = lower_tridiag_5;
	ab = new Float64Array([
		2.0,
		-1.0,  // col 0
		2.0,
		-1.0,  // col 1
		2.0,
		-1.0,  // col 2
		2.0,
		-1.0,  // col 3
		2.0,
		0.0   // col 4
	]);
	info = dpbtf2( 'lower', 5, 1, ab, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: upper_penta_4', function t() {
	var info;
	var tc;
	var ab;

	tc = upper_penta_4;
	ab = new Float64Array([
		0.0,
		0.0,
		4.0,   // col 0
		0.0,
		-1.0,
		4.0,   // col 1
		0.5,
		-1.0,
		4.0,   // col 2
		0.5,
		-1.0,
		4.0    // col 3
	]);
	info = dpbtf2( 'upper', 4, 2, ab, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: lower_penta_4', function t() {
	var info;
	var tc;
	var ab;

	tc = lower_penta_4;
	ab = new Float64Array([
		4.0,
		-1.0,
		0.5,   // col 0
		4.0,
		-1.0,
		0.5,   // col 1
		4.0,
		-1.0,
		0.0,   // col 2
		4.0,
		0.0,
		0.0    // col 3
	]);
	info = dpbtf2( 'lower', 4, 2, ab, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: n_one', function t() {
	var info;
	var tc;
	var ab;

	tc = n_one;
	ab = new Float64Array([ 9.0 ]);
	info = dpbtf2( 'upper', 1, 0, ab, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: n_zero', function t() {
	var info;
	var tc;
	var ab;

	tc = n_zero;
	ab = new Float64Array([ 99.0 ]);
	info = dpbtf2( 'lower', 0, 0, ab, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dpbtf2: not_posdef', function t() {
	var info;
	var tc;
	var ab;

	tc = not_posdef;
	ab = new Float64Array([
		1.0,
		2.0,   // col 0
		1.0,
		0.0    // col 1
	]);
	info = dpbtf2( 'lower', 2, 1, ab, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: upper_full_3', function t() {
	var info;
	var tc;
	var ab;

	tc = upper_full_3;
	ab = new Float64Array([
		0.0,
		0.0,
		4.0,   // col 0
		0.0,
		2.0,
		5.0,   // col 1
		1.0,
		3.0,
		6.0    // col 2
	]);
	info = dpbtf2( 'upper', 3, 2, ab, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( ab, tc.ab, 1e-14, 'ab' );
});

test( 'dpbtf2: not_posdef upper', function t() {
	var info;
	var ab;

	ab = new Float64Array([
		0.0,
		1.0,   // col 0: diag=1
		2.0,
		1.0    // col 1: superdiag=2, diag=1
	]);
	info = dpbtf2( 'upper', 2, 1, ab, 1, 2, 0 );
	assert.equal( info, 2 );
});
