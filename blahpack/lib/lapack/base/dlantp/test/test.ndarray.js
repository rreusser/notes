/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlantp = require( './../lib/base.js' );

// FIXTURES //

var dlantp_3x3_max_u_n = require( './fixtures/dlantp_3x3_max_u_n.json' );
var dlantp_3x3_one_u_n = require( './fixtures/dlantp_3x3_one_u_n.json' );
var dlantp_3x3_inf_u_n = require( './fixtures/dlantp_3x3_inf_u_n.json' );
var dlantp_3x3_frob_u_n = require( './fixtures/dlantp_3x3_frob_u_n.json' );
var dlantp_3x3_max_u_u = require( './fixtures/dlantp_3x3_max_u_u.json' );
var dlantp_3x3_one_u_u = require( './fixtures/dlantp_3x3_one_u_u.json' );
var dlantp_3x3_inf_u_u = require( './fixtures/dlantp_3x3_inf_u_u.json' );
var dlantp_3x3_frob_u_u = require( './fixtures/dlantp_3x3_frob_u_u.json' );
var dlantp_3x3_max_l_n = require( './fixtures/dlantp_3x3_max_l_n.json' );
var dlantp_3x3_one_l_n = require( './fixtures/dlantp_3x3_one_l_n.json' );
var dlantp_3x3_inf_l_n = require( './fixtures/dlantp_3x3_inf_l_n.json' );
var dlantp_3x3_frob_l_n = require( './fixtures/dlantp_3x3_frob_l_n.json' );
var dlantp_3x3_max_l_u = require( './fixtures/dlantp_3x3_max_l_u.json' );
var dlantp_3x3_one_l_u = require( './fixtures/dlantp_3x3_one_l_u.json' );
var dlantp_3x3_inf_l_u = require( './fixtures/dlantp_3x3_inf_l_u.json' );
var dlantp_3x3_frob_l_u = require( './fixtures/dlantp_3x3_frob_l_u.json' );
var dlantp_4x4_max_u_n = require( './fixtures/dlantp_4x4_max_u_n.json' );
var dlantp_4x4_one_u_n = require( './fixtures/dlantp_4x4_one_u_n.json' );
var dlantp_4x4_inf_u_n = require( './fixtures/dlantp_4x4_inf_u_n.json' );
var dlantp_4x4_frob_u_n = require( './fixtures/dlantp_4x4_frob_u_n.json' );
var dlantp_4x4_max_u_u = require( './fixtures/dlantp_4x4_max_u_u.json' );
var dlantp_4x4_one_u_u = require( './fixtures/dlantp_4x4_one_u_u.json' );
var dlantp_4x4_inf_u_u = require( './fixtures/dlantp_4x4_inf_u_u.json' );
var dlantp_4x4_frob_u_u = require( './fixtures/dlantp_4x4_frob_u_u.json' );
var dlantp_4x4_max_l_n = require( './fixtures/dlantp_4x4_max_l_n.json' );
var dlantp_4x4_one_l_n = require( './fixtures/dlantp_4x4_one_l_n.json' );
var dlantp_4x4_inf_l_n = require( './fixtures/dlantp_4x4_inf_l_n.json' );
var dlantp_4x4_frob_l_n = require( './fixtures/dlantp_4x4_frob_l_n.json' );
var dlantp_4x4_max_l_u = require( './fixtures/dlantp_4x4_max_l_u.json' );
var dlantp_4x4_one_l_u = require( './fixtures/dlantp_4x4_one_l_u.json' );
var dlantp_4x4_inf_l_u = require( './fixtures/dlantp_4x4_inf_l_u.json' );
var dlantp_4x4_frob_l_u = require( './fixtures/dlantp_4x4_frob_l_u.json' );
var dlantp_n0 = require( './fixtures/dlantp_n0.json' );
var dlantp_1x1_max_n = require( './fixtures/dlantp_1x1_max_n.json' );
var dlantp_1x1_one_n = require( './fixtures/dlantp_1x1_one_n.json' );
var dlantp_1x1_inf_n = require( './fixtures/dlantp_1x1_inf_n.json' );
var dlantp_1x1_frob_n = require( './fixtures/dlantp_1x1_frob_n.json' );
var dlantp_1x1_max_u = require( './fixtures/dlantp_1x1_max_u.json' );
var dlantp_1x1_one_u = require( './fixtures/dlantp_1x1_one_u.json' );
var dlantp_1x1_inf_u = require( './fixtures/dlantp_1x1_inf_u.json' );
var dlantp_1x1_frob_u = require( './fixtures/dlantp_1x1_frob_u.json' );

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

// TESTS //

test( 'dlantp is a function', function t() {
	assert.strictEqual( typeof dlantp, 'function' );
});

// 3x3 upper triangular matrix (non-unit diagonal):

//   A = [  2.0   3.0  -1.0 ]

//       [  0.0   5.0   2.0 ]

//       [  0.0   0.0   7.0 ]

// Upper packed: 2, 3, 5, -1, 2, 7

test( 'dlantp: dlantp_3x3_max_U_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_max_u_n;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'max', 'upper', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assert.strictEqual( typeof result, 'number', 'returns a number' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_3x3_one_U_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_one_u_n;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'one-norm', 'upper', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_3x3_inf_U_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_inf_u_n;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'inf-norm', 'upper', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_3x3_frob_U_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_frob_u_n;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'frobenius', 'upper', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 3x3 upper, unit diagonal

test( 'dlantp: dlantp_3x3_max_U_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_max_u_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'max', 'upper', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_3x3_one_U_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_one_u_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'one-norm', 'upper', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_3x3_inf_U_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_inf_u_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'inf-norm', 'upper', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_3x3_frob_U_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_frob_u_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'frobenius', 'upper', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 3x3 lower triangular matrix (non-unit diagonal):

//   A = [  2.0   0.0   0.0 ]

//       [  3.0   5.0   0.0 ]

//       [ -1.0   2.0   7.0 ]

// Lower packed: 2, 3, -1, 5, 2, 7

test( 'dlantp: dlantp_3x3_max_L_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_max_l_n;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 5.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'max', 'lower', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_3x3_one_L_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_one_l_n;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 5.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'one-norm', 'lower', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_3x3_inf_L_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_inf_l_n;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 5.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'inf-norm', 'lower', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_3x3_frob_L_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_frob_l_n;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 5.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'frobenius', 'lower', 'non-unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 3x3 lower, unit diagonal

test( 'dlantp: dlantp_3x3_max_L_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_max_l_u;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 5.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'max', 'lower', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_3x3_one_L_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_one_l_u;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 5.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'one-norm', 'lower', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_3x3_inf_L_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_inf_l_u;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 5.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'inf-norm', 'lower', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_3x3_frob_L_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_3x3_frob_l_u;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 5.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlantp( 'frobenius', 'lower', 'unit', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 4x4 upper triangular matrix (non-unit diagonal):

//   A = [  2.0   3.0  -1.0   4.0 ]

//       [  0.0   5.0   2.0  -6.0 ]

//       [  0.0   0.0   7.0   1.0 ]

//       [  0.0   0.0   0.0   8.0 ]

// Upper packed: 2, 3, 5, -1, 2, 7, 4, -6, 1, 8

test( 'dlantp: dlantp_4x4_max_U_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_max_u_n;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0, 4.0, -6.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'max', 'upper', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_4x4_one_U_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_one_u_n;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0, 4.0, -6.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'one-norm', 'upper', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_4x4_inf_U_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_inf_u_n;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0, 4.0, -6.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'inf-norm', 'upper', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_4x4_frob_U_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_frob_u_n;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0, 4.0, -6.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'frobenius', 'upper', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 4x4 upper, unit diagonal

test( 'dlantp: dlantp_4x4_max_U_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_max_u_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0, 4.0, -6.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'max', 'upper', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_4x4_one_U_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_one_u_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0, 4.0, -6.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'one-norm', 'upper', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_4x4_inf_U_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_inf_u_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0, 4.0, -6.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'inf-norm', 'upper', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_4x4_frob_U_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_frob_u_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0, 4.0, -6.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'frobenius', 'upper', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 4x4 lower triangular matrix (non-unit diagonal):

//   A = [  2.0   0.0   0.0   0.0 ]

//       [  3.0   5.0   0.0   0.0 ]

//       [ -1.0   2.0   7.0   0.0 ]

//       [  4.0  -6.0   1.0   8.0 ]

// Lower packed: 2, 3, -1, 4, 5, 2, -6, 7, 1, 8

test( 'dlantp: dlantp_4x4_max_L_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_max_l_n;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 4.0, 5.0, 2.0, -6.0, 7.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'max', 'lower', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_4x4_one_L_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_one_l_n;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 4.0, 5.0, 2.0, -6.0, 7.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'one-norm', 'lower', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_4x4_inf_L_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_inf_l_n;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 4.0, 5.0, 2.0, -6.0, 7.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'inf-norm', 'lower', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_4x4_frob_L_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_frob_l_n;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 4.0, 5.0, 2.0, -6.0, 7.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'frobenius', 'lower', 'non-unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 4x4 lower, unit diagonal

test( 'dlantp: dlantp_4x4_max_L_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_max_l_u;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 4.0, 5.0, 2.0, -6.0, 7.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'max', 'lower', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_4x4_one_L_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_one_l_u;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 4.0, 5.0, 2.0, -6.0, 7.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'one-norm', 'lower', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_4x4_inf_L_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_inf_l_u;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 4.0, 5.0, 2.0, -6.0, 7.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'inf-norm', 'lower', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_4x4_frob_L_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_4x4_frob_l_u;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 4.0, 5.0, 2.0, -6.0, 7.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlantp( 'frobenius', 'lower', 'unit', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=0 quick return
test( 'dlantp: dlantp_n0', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_n0;
	ap = new Float64Array( 0 );
	work = new Float64Array( 0 );
	result = dlantp( 'max', 'upper', 'non-unit', 0, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=1 matrix, single element = -5.5 (non-unit)
test( 'dlantp: dlantp_1x1_max_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_1x1_max_n;
	ap = new Float64Array( [ -5.5 ] );
	work = new Float64Array( 1 );
	result = dlantp( 'max', 'upper', 'non-unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_1x1_one_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_1x1_one_n;
	ap = new Float64Array( [ -5.5 ] );
	work = new Float64Array( 1 );
	result = dlantp( 'one-norm', 'upper', 'non-unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_1x1_inf_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_1x1_inf_n;
	ap = new Float64Array( [ -5.5 ] );
	work = new Float64Array( 1 );
	result = dlantp( 'inf-norm', 'upper', 'non-unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_1x1_frob_N', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_1x1_frob_n;
	ap = new Float64Array( [ -5.5 ] );
	work = new Float64Array( 1 );
	result = dlantp( 'frobenius', 'upper', 'non-unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=1, unit diagonal
test( 'dlantp: dlantp_1x1_max_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_1x1_max_u;
	ap = new Float64Array( [ -5.5 ] );
	work = new Float64Array( 1 );
	result = dlantp( 'max', 'upper', 'unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_1x1_one_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_1x1_one_u;
	ap = new Float64Array( [ -5.5 ] );
	work = new Float64Array( 1 );
	result = dlantp( 'one-norm', 'upper', 'unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_1x1_inf_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_1x1_inf_u;
	ap = new Float64Array( [ -5.5 ] );
	work = new Float64Array( 1 );
	result = dlantp( 'inf-norm', 'upper', 'unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlantp: dlantp_1x1_frob_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlantp_1x1_frob_u;
	ap = new Float64Array( [ -5.5 ] );
	work = new Float64Array( 1 );
	result = dlantp( 'frobenius', 'upper', 'unit', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
