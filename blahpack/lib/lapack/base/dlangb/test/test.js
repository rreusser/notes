/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlangb = require( './../lib/base.js' );

// FIXTURES //

var max_5x5 = require( './fixtures/max_5x5.json' );
var one_5x5 = require( './fixtures/one_5x5.json' );
var inf_5x5 = require( './fixtures/inf_5x5.json' );
var frob_5x5 = require( './fixtures/frob_5x5.json' );
var max_4x4_tridiag = require( './fixtures/max_4x4_tridiag.json' );
var one_4x4_tridiag = require( './fixtures/one_4x4_tridiag.json' );
var inf_4x4_tridiag = require( './fixtures/inf_4x4_tridiag.json' );
var frob_4x4_tridiag = require( './fixtures/frob_4x4_tridiag.json' );
var max_diag_only = require( './fixtures/max_diag_only.json' );
var one_diag_only = require( './fixtures/one_diag_only.json' );
var inf_diag_only = require( './fixtures/inf_diag_only.json' );
var frob_diag_only = require( './fixtures/frob_diag_only.json' );
var frob_1x1 = require( './fixtures/frob_1x1.json' );
var one_1x1 = require( './fixtures/one_1x1.json' );

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
* Creates the 5x5 general band matrix with KL=1, KU=2 (LDAB=4, column-major).
*
* Full matrix A:
*   A = [ 1   3   5   0   0 ]
*       [ 7   9   2   4   0 ]
*       [ 0   6   8   1   3 ]
*       [ 0   0   5   7   9 ]
*       [ 0   0   0   2   4 ]
*
* Band storage (LDAB=4):
*   Row 0 (2nd superdiag):  .    .    5    4    3
*   Row 1 (1st superdiag):  .    3    2    1    9
*   Row 2 (diagonal):       1    9    8    7    4
*   Row 3 (1st subdiag):    7    6    5    2    .
*
* @private
* @returns {Float64Array} band matrix
*/
function make5x5AB( ) {
	return new Float64Array([
		// Col 0: rows 0,1,2,3
		0,
		0,
		1,
		7,

		// Col 1: rows 0,1,2,3
		0,
		3,
		9,
		6,

		// Col 2: rows 0,1,2,3
		5,
		2,
		8,
		5,

		// Col 3: rows 0,1,2,3
		4,
		1,
		7,
		2,

		// Col 4: rows 0,1,2,3
		3,
		9,
		4,
		0
	]);
}

/**
* Creates the 4x4 tridiagonal band matrix KL=1, KU=1 (LDAB=3, column-major).
*
* Full matrix A:
*   A = [ 1   2   0   0 ]
*       [ 3   4   5   0 ]
*       [ 0   6   7   8 ]
*       [ 0   0   9   1 ]
*
* Band storage (LDAB=3):
*   Row 0 (superdiag):  .    2    5    8
*   Row 1 (diagonal):   1    4    7    1
*   Row 2 (subdiag):    3    6    9    .
*
* @private
* @returns {Float64Array} band matrix
*/
function make4x4AB( ) {
	return new Float64Array([
		// Col 0: rows 0,1,2
		0,
		1,
		3,

		// Col 1: rows 0,1,2
		2,
		4,
		6,

		// Col 2: rows 0,1,2
		5,
		7,
		9,

		// Col 3: rows 0,1,2
		8,
		1,
		0
	]);
}

// TESTS //

// --- 5x5 general band, KL=1, KU=2 ---

test( 'dlangb: max_5x5', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = max_5x5;
	AB = make5x5AB();
	WORK = new Float64Array( 10 );
	result = dlangb( 'max', 5, 1, 2, AB, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangb: one_5x5', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = one_5x5;
	AB = make5x5AB();
	WORK = new Float64Array( 10 );
	result = dlangb( 'one-norm', 5, 1, 2, AB, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangb: inf_5x5', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = inf_5x5;
	AB = make5x5AB();
	WORK = new Float64Array( 10 );
	result = dlangb( 'inf-norm', 5, 1, 2, AB, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangb: frob_5x5', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = frob_5x5;
	AB = make5x5AB();
	WORK = new Float64Array( 10 );
	result = dlangb( 'frobenius', 5, 1, 2, AB, 1, 4, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- 4x4 tridiagonal, KL=1, KU=1 ---

test( 'dlangb: max_4x4_tridiag', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = max_4x4_tridiag;
	AB = make4x4AB();
	WORK = new Float64Array( 10 );
	result = dlangb( 'max', 4, 1, 1, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangb: one_4x4_tridiag', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = one_4x4_tridiag;
	AB = make4x4AB();
	WORK = new Float64Array( 10 );
	result = dlangb( 'one-norm', 4, 1, 1, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangb: inf_4x4_tridiag', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = inf_4x4_tridiag;
	AB = make4x4AB();
	WORK = new Float64Array( 10 );
	result = dlangb( 'inf-norm', 4, 1, 1, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangb: frob_4x4_tridiag', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = frob_4x4_tridiag;
	AB = make4x4AB();
	WORK = new Float64Array( 10 );
	result = dlangb( 'frobenius', 4, 1, 1, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Edge cases ---

test( 'dlangb: n_zero', function t() {
	var result;
	var WORK;
	var AB;

	AB = new Float64Array( 1 );
	WORK = new Float64Array( 1 );
	result = dlangb( 'max', 0, 1, 2, AB, 1, 1, 0, WORK, 1, 0 );
	assert.equal( result, 0.0 );
});

test( 'dlangb: max_diag_only (KL=0, KU=0)', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = max_diag_only;
	AB = new Float64Array( [ 3, -1, 2 ] );
	WORK = new Float64Array( 10 );
	result = dlangb( 'max', 3, 0, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangb: one_diag_only (KL=0, KU=0)', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = one_diag_only;
	AB = new Float64Array( [ 3, -1, 2 ] );
	WORK = new Float64Array( 10 );
	result = dlangb( 'one-norm', 3, 0, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangb: inf_diag_only (KL=0, KU=0)', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = inf_diag_only;
	AB = new Float64Array( [ 3, -1, 2 ] );
	WORK = new Float64Array( 10 );
	result = dlangb( 'inf-norm', 3, 0, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangb: frob_diag_only (KL=0, KU=0)', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = frob_diag_only;
	AB = new Float64Array( [ 3, -1, 2 ] );
	WORK = new Float64Array( 10 );
	result = dlangb( 'frobenius', 3, 0, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangb: frob_1x1', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = frob_1x1;
	AB = new Float64Array( [ -5 ] );
	WORK = new Float64Array( 1 );
	result = dlangb( 'frobenius', 1, 0, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlangb: one_1x1', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = one_1x1;
	AB = new Float64Array( [ -5 ] );
	WORK = new Float64Array( 1 );
	result = dlangb( 'one-norm', 1, 0, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Unknown norm returns 0 ---

test( 'dlangb: unknown norm returns 0', function t() {
	var result;
	var WORK;
	var AB;

	AB = make5x5AB();
	WORK = new Float64Array( 10 );
	result = dlangb( 'unknown', 5, 1, 2, AB, 1, 4, 0, WORK, 1, 0 );
	assert.equal( result, 0.0 );
});
