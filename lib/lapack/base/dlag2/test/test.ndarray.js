/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var FLOAT64_SMALLEST_NORMAL = require( '@stdlib/constants/float64/smallest-normal' );
var dlag2 = require( './../lib/ndarray.js' );

// FIXTURES //

var real_eigenvalues_identity_b = require( './fixtures/real_eigenvalues_identity_b.json' );
var complex_eigenvalues = require( './fixtures/complex_eigenvalues.json' );
var diagonal = require( './fixtures/diagonal.json' );
var upper_tri_b = require( './fixtures/upper_tri_b.json' );
var small_b_diagonal = require( './fixtures/small_b_diagonal.json' );
var s1_leq_s2 = require( './fixtures/s1_leq_s2.json' );
var s1_gt_s2 = require( './fixtures/s1_gt_s2.json' );
var large_a = require( './fixtures/large_a.json' );
var negative_eigenvalues = require( './fixtures/negative_eigenvalues.json' );
var pp_gt_abi22 = require( './fixtures/pp_gt_abi22.json' );
var small_a = require( './fixtures/small_a.json' );
var both_b_diag_small = require( './fixtures/both_b_diag_small.json' );
var b_offdiag = require( './fixtures/b_offdiag.json' );
var large_eigenvalue_scaling = require( './fixtures/large_eigenvalue_scaling.json' );
var complex_nontrivial_b = require( './fixtures/complex_nontrivial_b.json' );
var negative_b_diag = require( './fixtures/negative_b_diag.json' );
var b22_small = require( './fixtures/b22_small.json' );
var large_pp = require( './fixtures/large_pp.json' );
var tiny_pp_qq = require( './fixtures/tiny_pp_qq.json' );
var identity = require( './fixtures/identity.json' );
var ascale_gt1_bsize_gt1 = require( './fixtures/ascale_gt1_bsize_gt1.json' );
var wsize_gt1_eigenvalue1 = require( './fixtures/wsize_gt1_eigenvalue1.json' );

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
* Helper to call dlag2 with column-major 2x2 matrices.
*
* @private
* @param {Array} aVals - [a11, a21, a12, a22] column-major
* @param {Array} bVals - [b11, b21, b12, b22] column-major
* @param {number} safmin - safe minimum
* @returns {Object} result with scale1, scale2, wr1, wr2, wi
*/
function callDlag2( aVals, bVals, safmin ) {
	var A = new Float64Array( aVals );
	var B = new Float64Array( bVals );
	return dlag2( A, 1, 2, 0, B, 1, 2, 0, safmin );
}

var SAFMIN = FLOAT64_SMALLEST_NORMAL;

// TESTS //

test( 'dlag2 is a function', function t() {
	assert.equal( typeof dlag2, 'function' );
});

test( 'dlag2: real_eigenvalues_identity_B', function t() {
	var result = callDlag2([ 4.0, 2.0, 1.0, 3.0 ], [ 1.0, 0.0, 0.0, 1.0 ], SAFMIN);
	var tc = real_eigenvalues_identity_b;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi should be zero for real eigenvalues' );
});

test( 'dlag2: complex_eigenvalues', function t() {
	var result = callDlag2([ 1.0, 2.0, -5.0, 1.0 ], [ 1.0, 0.0, 0.0, 1.0 ], SAFMIN);
	var tc = complex_eigenvalues;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assertClose( result.wi, tc.wi, 1e-14, 'wi' );
	assert.ok( result.wi > 0.0, 'wi should be positive for complex eigenvalues' );
});

test( 'dlag2: diagonal', function t() {
	var result = callDlag2([ 5.0, 0.0, 0.0, 3.0 ], [ 2.0, 0.0, 0.0, 1.0 ], SAFMIN);
	var tc = diagonal;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: upper_tri_B', function t() {
	var result = callDlag2([ 3.0, 1.0, 1.0, 2.0 ], [ 2.0, 0.0, 1.0, 3.0 ], SAFMIN);
	var tc = upper_tri_b;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: small_B_diagonal', function t() {
	var result = callDlag2([ 1.0, 1.0, 1.0, 1.0 ], [ 1.0e-200, 0.0, 0.0, 1.0 ], SAFMIN);
	var tc = small_b_diagonal;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-10, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: s1_leq_s2', function t() {
	var result = callDlag2([ 0.1, 2.0, 3.0, 5.0 ], [ 1.0, 0.0, 0.0, 1.0 ], SAFMIN);
	var tc = s1_leq_s2;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: s1_gt_s2', function t() {
	var result = callDlag2([ 10.0, 1.0, 1.0, 0.5 ], [ 1.0, 0.0, 0.0, 1.0 ], SAFMIN);
	var tc = s1_gt_s2;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: large_A', function t() {
	var result = callDlag2([ 1.0e100, 2.0e100, 3.0e100, 4.0e100 ], [ 1.0, 0.0, 0.0, 1.0 ], SAFMIN);
	var tc = large_a;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: negative_eigenvalues', function t() {
	var result = callDlag2([ -2.0, 1.0, 1.0, -3.0 ], [ 1.0, 0.0, 0.0, 1.0 ], SAFMIN);
	var tc = negative_eigenvalues;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: pp_gt_abi22', function t() {
	var result = callDlag2([ 6.0, 0.1, 1.0, 2.0 ], [ 1.0, 0.0, 0.0, 1.0 ], SAFMIN);
	var tc = pp_gt_abi22;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: small_A', function t() {
	var result = callDlag2([ 1.0e-200, 2.0e-200, 3.0e-200, 4.0e-200 ], [ 1.0, 0.0, 0.0, 1.0 ], SAFMIN);
	var tc = small_a;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: both_B_diag_small', function t() {
	var result = callDlag2([ 1.0, 0.5, 0.5, 1.0 ], [ 1.0e-200, 0.0, 0.0, 1.0e-200 ], SAFMIN);
	var tc = both_b_diag_small;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: B_offdiag', function t() {
	var result = callDlag2([ 2.0, 0.0, 0.0, 3.0 ], [ 1.0, 0.0, 0.5, 1.0 ], SAFMIN);
	var tc = b_offdiag;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: large_eigenvalue_scaling', function t() {
	var result = callDlag2([ 1.0e150, 0.0, 0.0, 2.0e150 ], [ 1.0e-10, 0.0, 0.0, 1.0e-10 ], SAFMIN);
	var tc = large_eigenvalue_scaling;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: complex_nontrivial_B', function t() {
	var result = callDlag2([ 1.0, 3.0, -2.0, 1.0 ], [ 2.0, 0.0, 1.0, 2.0 ], SAFMIN);
	var tc = complex_nontrivial_b;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assertClose( result.wi, tc.wi, 1e-14, 'wi' );
});

test( 'dlag2: negative_B_diag', function t() {
	var result = callDlag2([ 3.0, 1.0, 1.0, 2.0 ], [ -1.0, 0.0, 0.0, 1.0 ], SAFMIN);
	var tc = negative_b_diag;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: b22_small', function t() {
	var result = callDlag2([ 2.0, 1.0, 1.0, 3.0 ], [ 1.0, 0.0, 0.0, 1.0e-200 ], SAFMIN);
	var tc = b22_small;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-10, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: large_pp', function t() {
	var result = callDlag2([ 1.0, 0.0, 0.0, 1.0 ], [ 1.0e-155, 0.0, 0.0, 1.0e-155 ], SAFMIN);
	var tc = large_pp;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: tiny_pp_qq', function t() {
	var result = callDlag2([ 1.0e-200, 1.0e-200, 1.0e-200, 1.0e-200 ], [ 1.0, 0.0, 1.0, 1.0 ], SAFMIN);
	var tc = tiny_pp_qq;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: identity', function t() {
	var result = callDlag2([ 1.0, 0.0, 0.0, 1.0 ], [ 1.0, 0.0, 0.0, 1.0 ], SAFMIN);
	var tc = identity;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: ascale_gt1_bsize_gt1', function t() {
	var result = callDlag2([ 0.4, 0.0, 0.0, 0.3 ], [ 2.0, 0.0, 0.0, 3.0 ], SAFMIN);
	var tc = ascale_gt1_bsize_gt1;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: wsize_gt1_eigenvalue1', function t() {
	var result = callDlag2([ 1.0, 0.0, 0.0, 0.5 ], [ 1.0e-100, 0.0, 0.0, 1.0e-100 ], SAFMIN);
	var tc = wsize_gt1_eigenvalue1;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: supports non-unit strides', function t() {
	var result;
	var tc;
	var A;
	var B;

	A = new Float64Array( [ 4.0, -1.0, 2.0, -1.0, 1.0, -1.0, 3.0, -1.0 ] );
	B = new Float64Array( [ 1.0, -1.0, 0.0, -1.0, 0.0, -1.0, 1.0, -1.0 ] );
	result = dlag2( A, 2, 4, 0, B, 2, 4, 0, SAFMIN );
	tc = real_eigenvalues_identity_b;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});

test( 'dlag2: supports offsets', function t() {
	var result;
	var tc;
	var A;
	var B;

	A = new Float64Array( [ -1.0, -1.0, 4.0, 2.0, 1.0, 3.0 ] );
	B = new Float64Array( [ -1.0, -1.0, 1.0, 0.0, 0.0, 1.0 ] );
	result = dlag2( A, 1, 2, 2, B, 1, 2, 2, SAFMIN );
	tc = real_eigenvalues_identity_b;
	assertClose( result.scale1, tc.scale1, 1e-14, 'scale1' );
	assertClose( result.scale2, tc.scale2, 1e-14, 'scale2' );
	assertClose( result.wr1, tc.wr1, 1e-14, 'wr1' );
	assertClose( result.wr2, tc.wr2, 1e-14, 'wr2' );
	assert.equal( result.wi, tc.wi, 'wi' );
});
