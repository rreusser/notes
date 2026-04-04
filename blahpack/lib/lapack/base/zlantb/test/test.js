/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlantb = require( './../lib/base.js' );

// FIXTURES //

var max_upper_nonunit = require( './fixtures/max_upper_nonunit.json' );
var max_upper_unit = require( './fixtures/max_upper_unit.json' );
var one_upper_nonunit = require( './fixtures/one_upper_nonunit.json' );
var one_upper_unit = require( './fixtures/one_upper_unit.json' );
var inf_upper_nonunit = require( './fixtures/inf_upper_nonunit.json' );
var inf_upper_unit = require( './fixtures/inf_upper_unit.json' );
var frob_upper_nonunit = require( './fixtures/frob_upper_nonunit.json' );
var frob_upper_unit = require( './fixtures/frob_upper_unit.json' );
var max_lower_nonunit = require( './fixtures/max_lower_nonunit.json' );
var max_lower_unit = require( './fixtures/max_lower_unit.json' );
var one_lower_nonunit = require( './fixtures/one_lower_nonunit.json' );
var one_lower_unit = require( './fixtures/one_lower_unit.json' );
var inf_lower_nonunit = require( './fixtures/inf_lower_nonunit.json' );
var inf_lower_unit = require( './fixtures/inf_lower_unit.json' );
var frob_lower_nonunit = require( './fixtures/frob_lower_nonunit.json' );
var frob_lower_unit = require( './fixtures/frob_lower_unit.json' );
var max_k0_nonunit = require( './fixtures/max_k0_nonunit.json' );
var frob_k0_unit = require( './fixtures/frob_k0_unit.json' );
var frob_1x1 = require( './fixtures/frob_1x1.json' );
var one_k1_upper_nonunit = require( './fixtures/one_k1_upper_nonunit.json' );
var inf_k1_lower_unit = require( './fixtures/inf_k1_lower_unit.json' );

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
* Creates the 4x4 upper triangular band matrix (K=2, LDAB=3, column-major).
*
* @private
* @returns {Complex128Array} band matrix
*/
function makeUpperAB( ) {
	return new Complex128Array([
		// Col 0: rows 0,1,2
		0,
		0,
		0,
		0,
		1,
		2,

		// Col 1: rows 0,1,2
		0,
		0,
		3,
		4,
		7,
		8,

		// Col 2: rows 0,1,2
		5,
		6,
		9,
		1,
		4,
		5,

		// Col 3: rows 0,1,2
		2,
		3,
		6,
		7,
		8,
		9
	]);
}

/**
* Creates the 4x4 lower triangular band matrix (K=2, LDAB=3, column-major).
*
* @private
* @returns {Complex128Array} band matrix
*/
function makeLowerAB( ) {
	return new Complex128Array([
		// Col 0: rows 0,1,2
		1,
		2,
		3,
		4,
		5,
		6,

		// Col 1: rows 0,1,2
		7,
		8,
		9,
		1,
		2,
		3,

		// Col 2: rows 0,1,2
		4,
		5,
		6,
		7,
		0,
		0,

		// Col 3: rows 0,1,2
		8,
		9,
		0,
		0,
		0,
		0
	]);
}

// TESTS //

// --- Upper triangular, K=2 ---

test( 'zlantb: max_upper_nonunit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = max_upper_nonunit;
	AB = makeUpperAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'max', 'upper', 'non-unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: max_upper_unit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = max_upper_unit;
	AB = makeUpperAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'max', 'upper', 'unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: one_upper_nonunit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = one_upper_nonunit;
	AB = makeUpperAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'one-norm', 'upper', 'non-unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: one_upper_unit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = one_upper_unit;
	AB = makeUpperAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'one-norm', 'upper', 'unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: inf_upper_nonunit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = inf_upper_nonunit;
	AB = makeUpperAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'inf-norm', 'upper', 'non-unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: inf_upper_unit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = inf_upper_unit;
	AB = makeUpperAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'inf-norm', 'upper', 'unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: frob_upper_nonunit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = frob_upper_nonunit;
	AB = makeUpperAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'frobenius', 'upper', 'non-unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: frob_upper_unit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = frob_upper_unit;
	AB = makeUpperAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'frobenius', 'upper', 'unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Lower triangular, K=2 ---

test( 'zlantb: max_lower_nonunit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = max_lower_nonunit;
	AB = makeLowerAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'max', 'lower', 'non-unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: max_lower_unit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = max_lower_unit;
	AB = makeLowerAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'max', 'lower', 'unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: one_lower_nonunit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = one_lower_nonunit;
	AB = makeLowerAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'one-norm', 'lower', 'non-unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: one_lower_unit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = one_lower_unit;
	AB = makeLowerAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'one-norm', 'lower', 'unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: inf_lower_nonunit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = inf_lower_nonunit;
	AB = makeLowerAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'inf-norm', 'lower', 'non-unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: inf_lower_unit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = inf_lower_unit;
	AB = makeLowerAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'inf-norm', 'lower', 'unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: frob_lower_nonunit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = frob_lower_nonunit;
	AB = makeLowerAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'frobenius', 'lower', 'non-unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: frob_lower_unit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = frob_lower_unit;
	AB = makeLowerAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'frobenius', 'lower', 'unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Edge cases ---

test( 'zlantb: n_zero', function t() {
	var result;
	var WORK;
	var AB;

	AB = new Complex128Array( 1 );
	WORK = new Float64Array( 1 );
	result = zlantb( 'max', 'upper', 'non-unit', 0, 2, AB, 1, 1, 0, WORK, 1, 0 );
	assert.equal( result, 0.0 );
});

test( 'zlantb: max_k0_nonunit (diagonal only)', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = max_k0_nonunit;
	AB = new Complex128Array([
		3, 4, 1, 1, 2, 2
	]);
	WORK = new Float64Array( 10 );
	result = zlantb( 'max', 'upper', 'non-unit', 3, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: frob_k0_unit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = frob_k0_unit;
	AB = new Complex128Array([
		3, 4, 1, 1, 2, 2
	]);
	WORK = new Float64Array( 10 );
	result = zlantb( 'frobenius', 'upper', 'unit', 3, 0, AB, 1, 1, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: frob_1x1', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = frob_1x1;
	AB = new Complex128Array( [ 3, 4 ] );
	WORK = new Float64Array( 1 );
	result = zlantb( 'frobenius', 'upper', 'non-unit', 1, 0, AB, 1, 1, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: one_k1_upper_nonunit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = one_k1_upper_nonunit;
	AB = new Complex128Array([
		// Col 0: rows 0,1
		0,
		0,
		1,
		1,

		// Col 1: rows 0,1
		2,
		2,
		3,
		3,

		// Col 2: rows 0,1
		4,
		4,
		5,
		5
	]);
	WORK = new Float64Array( 10 );
	result = zlantb( 'one-norm', 'upper', 'non-unit', 3, 1, AB, 1, 2, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlantb: inf_k1_lower_unit', function t() {
	var result;
	var WORK;
	var tc;
	var AB;

	tc = inf_k1_lower_unit;
	AB = new Complex128Array([
		// Col 0: rows 0,1
		99,
		99,
		2,
		2,

		// Col 1: rows 0,1
		99,
		99,
		4,
		4,

		// Col 2: rows 0,1
		99,
		99,
		0,
		0
	]);
	WORK = new Float64Array( 10 );
	result = zlantb( 'inf-norm', 'lower', 'unit', 3, 1, AB, 1, 2, 0, WORK, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Unknown norm returns 0 ---

test( 'zlantb: unknown norm returns 0', function t() {
	var result;
	var WORK;
	var AB;

	AB = makeUpperAB();
	WORK = new Float64Array( 10 );
	result = zlantb( 'unknown', 'upper', 'non-unit', 4, 2, AB, 1, 3, 0, WORK, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result, 0.0 );
});
