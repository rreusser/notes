/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlanhp = require( './../lib/base.js' );

// FIXTURES //

var zlanhp_3x3_max_u = require( './fixtures/zlanhp_3x3_max_u.json' );
var zlanhp_3x3_one_u = require( './fixtures/zlanhp_3x3_one_u.json' );
var zlanhp_3x3_inf_u = require( './fixtures/zlanhp_3x3_inf_u.json' );
var zlanhp_3x3_frob_u = require( './fixtures/zlanhp_3x3_frob_u.json' );
var zlanhp_3x3_max_l = require( './fixtures/zlanhp_3x3_max_l.json' );
var zlanhp_3x3_one_l = require( './fixtures/zlanhp_3x3_one_l.json' );
var zlanhp_3x3_inf_l = require( './fixtures/zlanhp_3x3_inf_l.json' );
var zlanhp_3x3_frob_l = require( './fixtures/zlanhp_3x3_frob_l.json' );
var zlanhp_4x4_max_u = require( './fixtures/zlanhp_4x4_max_u.json' );
var zlanhp_4x4_one_u = require( './fixtures/zlanhp_4x4_one_u.json' );
var zlanhp_4x4_inf_u = require( './fixtures/zlanhp_4x4_inf_u.json' );
var zlanhp_4x4_frob_u = require( './fixtures/zlanhp_4x4_frob_u.json' );
var zlanhp_4x4_max_l = require( './fixtures/zlanhp_4x4_max_l.json' );
var zlanhp_4x4_one_l = require( './fixtures/zlanhp_4x4_one_l.json' );
var zlanhp_4x4_inf_l = require( './fixtures/zlanhp_4x4_inf_l.json' );
var zlanhp_4x4_frob_l = require( './fixtures/zlanhp_4x4_frob_l.json' );
var zlanhp_n0 = require( './fixtures/zlanhp_n0.json' );
var zlanhp_1x1_max = require( './fixtures/zlanhp_1x1_max.json' );
var zlanhp_1x1_one = require( './fixtures/zlanhp_1x1_one.json' );
var zlanhp_1x1_inf = require( './fixtures/zlanhp_1x1_inf.json' );
var zlanhp_1x1_frob = require( './fixtures/zlanhp_1x1_frob.json' );
var zlanhp_1x1_neg_max = require( './fixtures/zlanhp_1x1_neg_max.json' );
var zlanhp_1x1_neg_frob = require( './fixtures/zlanhp_1x1_neg_frob.json' );
// 3x3 Hermitian matrix (upper packed, interleaved re/im):
var AP3U = [ 2.0, 0.0, 1.0, 2.0, 5.0, 0.0, -1.0, 3.0, 0.5, -1.5, 7.0, 0.0 ]; // eslint-disable-line max-len

// 3x3 Hermitian matrix (lower packed, interleaved re/im):
var AP3L = [ 2.0, 0.0, 1.0, -2.0, -1.0, -3.0, 5.0, 0.0, 0.5, 1.5, 7.0, 0.0 ]; // eslint-disable-line max-len

// 4x4 Hermitian matrix (upper packed, interleaved re/im):
var AP4U = [ 2.0, 0.0, 3.0, 1.0, 5.0, 0.0, -1.0, 2.0, 2.0, 0.5, 7.0, 0.0, 4.0, -1.0, -6.0, 3.0, 1.0, 1.0, 8.0, 0.0 ]; // eslint-disable-line max-len

// 4x4 Hermitian matrix (lower packed, interleaved re/im):
var AP4L = [ 2.0, 0.0, 3.0, -1.0, -1.0, -2.0, 4.0, 1.0, 5.0, 0.0, 2.0, -0.5, -6.0, -3.0, 7.0, 0.0, 1.0, -1.0, 8.0, 0.0 ]; // eslint-disable-line max-len

// FUNCTIONS //

/**
* Creates a Complex128Array from interleaved real/imaginary pairs.
*
* @private
* @param {Array} arr - interleaved values
* @returns {Complex128Array} complex array
*/
function c128( arr ) {
	return new Complex128Array( arr );
}

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

// TESTS //

test( 'zlanhp is a function', function t() {
	assert.strictEqual( typeof zlanhp, 'function' );
});

test( 'zlanhp: zlanhp_3x3_max_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_3x3_max_u;
	ap = c128( AP3U );
	work = new Float64Array( 3 );
	result = zlanhp( 'max', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assert.strictEqual( typeof result, 'number', 'returns a number' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_3x3_one_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_3x3_one_u;
	ap = c128( AP3U );
	work = new Float64Array( 3 );
	result = zlanhp( 'one-norm', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_3x3_inf_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_3x3_inf_u;
	ap = c128( AP3U );
	work = new Float64Array( 3 );
	result = zlanhp( 'inf-norm', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_3x3_frob_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_3x3_frob_u;
	ap = c128( AP3U );
	work = new Float64Array( 3 );
	result = zlanhp( 'frobenius', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_3x3_max_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_3x3_max_l;
	ap = c128( AP3L );
	work = new Float64Array( 3 );
	result = zlanhp( 'max', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_3x3_one_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_3x3_one_l;
	ap = c128( AP3L );
	work = new Float64Array( 3 );
	result = zlanhp( 'one-norm', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_3x3_inf_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_3x3_inf_l;
	ap = c128( AP3L );
	work = new Float64Array( 3 );
	result = zlanhp( 'inf-norm', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_3x3_frob_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_3x3_frob_l;
	ap = c128( AP3L );
	work = new Float64Array( 3 );
	result = zlanhp( 'frobenius', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_4x4_max_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_4x4_max_u;
	ap = c128( AP4U );
	work = new Float64Array( 4 );
	result = zlanhp( 'max', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_4x4_one_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_4x4_one_u;
	ap = c128( AP4U );
	work = new Float64Array( 4 );
	result = zlanhp( 'one-norm', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_4x4_inf_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_4x4_inf_u;
	ap = c128( AP4U );
	work = new Float64Array( 4 );
	result = zlanhp( 'inf-norm', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_4x4_frob_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_4x4_frob_u;
	ap = c128( AP4U );
	work = new Float64Array( 4 );
	result = zlanhp( 'frobenius', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_4x4_max_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_4x4_max_l;
	ap = c128( AP4L );
	work = new Float64Array( 4 );
	result = zlanhp( 'max', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_4x4_one_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_4x4_one_l;
	ap = c128( AP4L );
	work = new Float64Array( 4 );
	result = zlanhp( 'one-norm', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_4x4_inf_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_4x4_inf_l;
	ap = c128( AP4L );
	work = new Float64Array( 4 );
	result = zlanhp( 'inf-norm', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_4x4_frob_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_4x4_frob_l;
	ap = c128( AP4L );
	work = new Float64Array( 4 );
	result = zlanhp( 'frobenius', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=0 quick return
test( 'zlanhp: zlanhp_n0', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_n0;
	ap = c128( [] );
	work = new Float64Array( 0 );
	result = zlanhp( 'max', 'upper', 0, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=1 matrix, single element = (3.5, 0.0)
test( 'zlanhp: zlanhp_1x1_max', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_1x1_max;
	ap = c128( [ 3.5, 0.0 ] );
	work = new Float64Array( 1 );
	result = zlanhp( 'max', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_1x1_one', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_1x1_one;
	ap = c128( [ 3.5, 0.0 ] );
	work = new Float64Array( 1 );
	result = zlanhp( 'one-norm', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_1x1_inf', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_1x1_inf;
	ap = c128( [ 3.5, 0.0 ] );
	work = new Float64Array( 1 );
	result = zlanhp( 'inf-norm', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_1x1_frob', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_1x1_frob;
	ap = c128( [ 3.5, 0.0 ] );
	work = new Float64Array( 1 );
	result = zlanhp( 'frobenius', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=1 matrix with negative diagonal = (-5.5, 0.0)
test( 'zlanhp: zlanhp_1x1_neg_max', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_1x1_neg_max;
	ap = c128( [ -5.5, 0.0 ] );
	work = new Float64Array( 1 );
	result = zlanhp( 'max', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanhp: zlanhp_1x1_neg_frob', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlanhp_1x1_neg_frob;
	ap = c128( [ -5.5, 0.0 ] );
	work = new Float64Array( 1 );
	result = zlanhp( 'frobenius', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
