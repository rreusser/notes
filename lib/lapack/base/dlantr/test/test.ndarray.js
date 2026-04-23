/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlantr = require( './../lib/base.js' );

// FIXTURES //

var upper_nonunit_max = require( './fixtures/upper_nonunit_max.json' );
var upper_nonunit_one = require( './fixtures/upper_nonunit_one.json' );
var upper_nonunit_inf = require( './fixtures/upper_nonunit_inf.json' );
var upper_nonunit_frob = require( './fixtures/upper_nonunit_frob.json' );
var upper_unit_max = require( './fixtures/upper_unit_max.json' );
var upper_unit_one = require( './fixtures/upper_unit_one.json' );
var upper_unit_inf = require( './fixtures/upper_unit_inf.json' );
var upper_unit_frob = require( './fixtures/upper_unit_frob.json' );
var lower_nonunit_max = require( './fixtures/lower_nonunit_max.json' );
var lower_nonunit_one = require( './fixtures/lower_nonunit_one.json' );
var lower_nonunit_inf = require( './fixtures/lower_nonunit_inf.json' );
var lower_nonunit_frob = require( './fixtures/lower_nonunit_frob.json' );
var lower_unit_max = require( './fixtures/lower_unit_max.json' );
var lower_unit_one = require( './fixtures/lower_unit_one.json' );
var lower_unit_inf = require( './fixtures/lower_unit_inf.json' );
var lower_unit_frob = require( './fixtures/lower_unit_frob.json' );
var rect_upper_mn_max = require( './fixtures/rect_upper_mn_max.json' );
var rect_upper_mn_one = require( './fixtures/rect_upper_mn_one.json' );
var rect_upper_mn_inf = require( './fixtures/rect_upper_mn_inf.json' );
var rect_upper_mn_frob = require( './fixtures/rect_upper_mn_frob.json' );
var rect_upper_nm_max = require( './fixtures/rect_upper_nm_max.json' );
var rect_upper_nm_one = require( './fixtures/rect_upper_nm_one.json' );
var rect_upper_nm_inf = require( './fixtures/rect_upper_nm_inf.json' );
var rect_upper_nm_frob = require( './fixtures/rect_upper_nm_frob.json' );
var rect_lower_mn_max = require( './fixtures/rect_lower_mn_max.json' );
var rect_lower_mn_one = require( './fixtures/rect_lower_mn_one.json' );
var rect_lower_mn_inf = require( './fixtures/rect_lower_mn_inf.json' );
var rect_lower_mn_frob = require( './fixtures/rect_lower_mn_frob.json' );
var edge_1x1_max = require( './fixtures/edge_1x1_max.json' );
var edge_1x1_one = require( './fixtures/edge_1x1_one.json' );
var edge_1x1_inf = require( './fixtures/edge_1x1_inf.json' );
var edge_1x1_frob = require( './fixtures/edge_1x1_frob.json' );
var edge_1x1_unit_max = require( './fixtures/edge_1x1_unit_max.json' );

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

// FIXTURES DATA //

// Upper triangular 4x4 matrix (column-major, LDA=4):
//   [ 1  -4   7  -2 ]
//   [ 0   5  -8   6 ]
//   [ 0   0   9  -3 ]
//   [ 0   0   0   4 ]
var upperA = new Float64Array([
	1,
	0,
	0,
	0,   // col 0
	-4,
	5,
	0,
	0,  // col 1
	7,
	-8,
	9,
	0,  // col 2
	-2,
	6,
	-3,
	4  // col 3
]);

// Lower triangular 4x4 matrix (column-major, LDA=4):

//   [ 2   0   0   0 ]

//   [-3   6   0   0 ]

//   [ 1  -5   8   0 ]

//   [-4   7  -2   3 ]
var lowerA = new Float64Array([
	2,
	-3,
	1,
	-4,  // col 0
	0,
	6,
	-5,
	7,   // col 1
	0,
	0,
	8,
	-2,   // col 2
	0,
	0,
	0,
	3     // col 3
]);

// Rectangular upper M>N: 3x2 (LDA=3)

//   [ 1  -4 ]

//   [ 0   5 ]

//   [ 0   0 ]
var rectUpperMN = new Float64Array([
	1,
	0,
	0,    // col 0
	-4,
	5,
	0    // col 1
]);

// Rectangular upper M<N: 2x4 (LDA=2)

//   [ 1  -4   7  -2 ]

//   [ 0   5  -8   6 ]
var rectUpperNM = new Float64Array([
	1,
	0,       // col 0
	-4,
	5,      // col 1
	7,
	-8,      // col 2
	-2,
	6       // col 3
]);

// Rectangular lower M>N: 4x2 (LDA=4)

//   [ 2   0 ]

//   [-3   6 ]

//   [ 1  -5 ]

//   [-4   7 ]
var rectLowerMN = new Float64Array([
	2,
	-3,
	1,
	-4,  // col 0
	0,
	6,
	-5,
	7    // col 1
]);

// 1x1 matrix
var oneByOne = new Float64Array([ 5.0 ]);

var work = new Float64Array( 10 );

// TESTS //

// --- Upper triangular, non-unit diagonal ---

test( 'dlantr: upper_nonunit_max', function t() {
	var result = dlantr( 'max', 'upper', 'non-unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	var tc = upper_nonunit_max; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: upper_nonunit_one', function t() {
	var result = dlantr( 'one-norm', 'upper', 'non-unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	var tc = upper_nonunit_one; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: upper_nonunit_inf', function t() {
	var result = dlantr( 'inf-norm', 'upper', 'non-unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	var tc = upper_nonunit_inf; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: upper_nonunit_frob', function t() {
	var result = dlantr( 'frobenius', 'upper', 'non-unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	var tc = upper_nonunit_frob; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Upper triangular, unit diagonal ---

test( 'dlantr: upper_unit_max', function t() {
	var result = dlantr( 'max', 'upper', 'unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	var tc = upper_unit_max; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: upper_unit_one', function t() {
	var result = dlantr( 'one-norm', 'upper', 'unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	var tc = upper_unit_one; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: upper_unit_inf', function t() {
	var result = dlantr( 'inf-norm', 'upper', 'unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	var tc = upper_unit_inf; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: upper_unit_frob', function t() {
	var result = dlantr( 'frobenius', 'upper', 'unit', 4, 4, upperA, 1, 4, 0, work, 1, 0 );
	var tc = upper_unit_frob; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Lower triangular, non-unit diagonal ---

test( 'dlantr: lower_nonunit_max', function t() {
	var result = dlantr( 'max', 'lower', 'non-unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	var tc = lower_nonunit_max; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: lower_nonunit_one', function t() {
	var result = dlantr( 'one-norm', 'lower', 'non-unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	var tc = lower_nonunit_one; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: lower_nonunit_inf', function t() {
	var result = dlantr( 'inf-norm', 'lower', 'non-unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	var tc = lower_nonunit_inf; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: lower_nonunit_frob', function t() {
	var result = dlantr( 'frobenius', 'lower', 'non-unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	var tc = lower_nonunit_frob; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Lower triangular, unit diagonal ---

test( 'dlantr: lower_unit_max', function t() {
	var result = dlantr( 'max', 'lower', 'unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	var tc = lower_unit_max; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: lower_unit_one', function t() {
	var result = dlantr( 'one-norm', 'lower', 'unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	var tc = lower_unit_one; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: lower_unit_inf', function t() {
	var result = dlantr( 'inf-norm', 'lower', 'unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	var tc = lower_unit_inf; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: lower_unit_frob', function t() {
	var result = dlantr( 'frobenius', 'lower', 'unit', 4, 4, lowerA, 1, 4, 0, work, 1, 0 );
	var tc = lower_unit_frob; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Edge cases ---

test( 'dlantr: edge_m0', function t() {
	var result = dlantr( 'max', 'upper', 'non-unit', 0, 4, upperA, 1, 4, 0, work, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result, 0.0 );
});

test( 'dlantr: edge_n0', function t() {
	var result = dlantr( 'max', 'upper', 'non-unit', 4, 0, upperA, 1, 4, 0, work, 1, 0 ); // eslint-disable-line max-len
	assert.equal( result, 0.0 );
});

// --- Rectangular upper, M > N (3x2) ---

test( 'dlantr: rect_upper_mn_max', function t() {
	var result = dlantr( 'max', 'upper', 'non-unit', 3, 2, rectUpperMN, 1, 3, 0, work, 1, 0 );
	var tc = rect_upper_mn_max; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_upper_mn_one', function t() {
	var result = dlantr( 'one-norm', 'upper', 'non-unit', 3, 2, rectUpperMN, 1, 3, 0, work, 1, 0 );
	var tc = rect_upper_mn_one; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_upper_mn_inf', function t() {
	var result = dlantr( 'inf-norm', 'upper', 'non-unit', 3, 2, rectUpperMN, 1, 3, 0, work, 1, 0 );
	var tc = rect_upper_mn_inf; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_upper_mn_frob', function t() {
	var result = dlantr( 'frobenius', 'upper', 'non-unit', 3, 2, rectUpperMN, 1, 3, 0, work, 1, 0 );
	var tc = rect_upper_mn_frob; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Rectangular upper, M < N (2x4) ---

test( 'dlantr: rect_upper_nm_max', function t() {
	var result = dlantr( 'max', 'upper', 'non-unit', 2, 4, rectUpperNM, 1, 2, 0, work, 1, 0 );
	var tc = rect_upper_nm_max; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_upper_nm_one', function t() {
	var result = dlantr( 'one-norm', 'upper', 'non-unit', 2, 4, rectUpperNM, 1, 2, 0, work, 1, 0 );
	var tc = rect_upper_nm_one; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_upper_nm_inf', function t() {
	var result = dlantr( 'inf-norm', 'upper', 'non-unit', 2, 4, rectUpperNM, 1, 2, 0, work, 1, 0 );
	var tc = rect_upper_nm_inf; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_upper_nm_frob', function t() {
	var result = dlantr( 'frobenius', 'upper', 'non-unit', 2, 4, rectUpperNM, 1, 2, 0, work, 1, 0 );
	var tc = rect_upper_nm_frob; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- Rectangular lower, M > N (4x2) ---

test( 'dlantr: rect_lower_mn_max', function t() {
	var result = dlantr( 'max', 'lower', 'non-unit', 4, 2, rectLowerMN, 1, 4, 0, work, 1, 0 );
	var tc = rect_lower_mn_max; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_lower_mn_one', function t() {
	var result = dlantr( 'one-norm', 'lower', 'non-unit', 4, 2, rectLowerMN, 1, 4, 0, work, 1, 0 );
	var tc = rect_lower_mn_one; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_lower_mn_inf', function t() {
	var result = dlantr( 'inf-norm', 'lower', 'non-unit', 4, 2, rectLowerMN, 1, 4, 0, work, 1, 0 );
	var tc = rect_lower_mn_inf; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: rect_lower_mn_frob', function t() {
	var result = dlantr( 'frobenius', 'lower', 'non-unit', 4, 2, rectLowerMN, 1, 4, 0, work, 1, 0 );
	var tc = rect_lower_mn_frob; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

// --- 1x1 matrix ---

test( 'dlantr: edge_1x1_max', function t() {
	var result = dlantr( 'max', 'upper', 'non-unit', 1, 1, oneByOne, 1, 1, 0, work, 1, 0 );
	var tc = edge_1x1_max; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: edge_1x1_one', function t() {
	var result = dlantr( 'one-norm', 'upper', 'non-unit', 1, 1, oneByOne, 1, 1, 0, work, 1, 0 );
	var tc = edge_1x1_one; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: edge_1x1_inf', function t() {
	var result = dlantr( 'inf-norm', 'upper', 'non-unit', 1, 1, oneByOne, 1, 1, 0, work, 1, 0 );
	var tc = edge_1x1_inf; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: edge_1x1_frob', function t() {
	var result = dlantr( 'frobenius', 'upper', 'non-unit', 1, 1, oneByOne, 1, 1, 0, work, 1, 0 );
	var tc = edge_1x1_frob; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantr: edge_1x1_unit_max', function t() {
	var result = dlantr( 'max', 'upper', 'unit', 1, 1, oneByOne, 1, 1, 0, work, 1, 0 );
	var tc = edge_1x1_unit_max; // eslint-disable-line max-len
	assertClose( result, tc.result, 1e-14, 'result' );
});
