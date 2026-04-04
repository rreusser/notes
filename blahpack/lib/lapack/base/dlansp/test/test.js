/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlansp = require( './../lib/base.js' );

// FIXTURES //

var dlansp_3x3_max_u = require( './fixtures/dlansp_3x3_max_u.json' );
var dlansp_3x3_one_u = require( './fixtures/dlansp_3x3_one_u.json' );
var dlansp_3x3_inf_u = require( './fixtures/dlansp_3x3_inf_u.json' );
var dlansp_3x3_frob_u = require( './fixtures/dlansp_3x3_frob_u.json' );
var dlansp_3x3_max_l = require( './fixtures/dlansp_3x3_max_l.json' );
var dlansp_3x3_one_l = require( './fixtures/dlansp_3x3_one_l.json' );
var dlansp_3x3_inf_l = require( './fixtures/dlansp_3x3_inf_l.json' );
var dlansp_3x3_frob_l = require( './fixtures/dlansp_3x3_frob_l.json' );
var dlansp_4x4_max_u = require( './fixtures/dlansp_4x4_max_u.json' );
var dlansp_4x4_one_u = require( './fixtures/dlansp_4x4_one_u.json' );
var dlansp_4x4_inf_u = require( './fixtures/dlansp_4x4_inf_u.json' );
var dlansp_4x4_frob_u = require( './fixtures/dlansp_4x4_frob_u.json' );
var dlansp_4x4_max_l = require( './fixtures/dlansp_4x4_max_l.json' );
var dlansp_4x4_one_l = require( './fixtures/dlansp_4x4_one_l.json' );
var dlansp_4x4_inf_l = require( './fixtures/dlansp_4x4_inf_l.json' );
var dlansp_4x4_frob_l = require( './fixtures/dlansp_4x4_frob_l.json' );
var dlansp_n0 = require( './fixtures/dlansp_n0.json' );
var dlansp_1x1_max = require( './fixtures/dlansp_1x1_max.json' );
var dlansp_1x1_one = require( './fixtures/dlansp_1x1_one.json' );
var dlansp_1x1_inf = require( './fixtures/dlansp_1x1_inf.json' );
var dlansp_1x1_frob = require( './fixtures/dlansp_1x1_frob.json' );

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

// 3x3 symmetric matrix:
//   A = [  2.0   3.0  -1.0 ]
//       [  3.0   5.0   2.0 ]
//       [ -1.0   2.0   7.0 ]

// Upper packed: 2, 3, 5, -1, 2, 7
// Lower packed: 2, 3, -1, 5, 2, 7

test( 'dlansp is a function', function t() {
	assert.strictEqual( typeof dlansp, 'function' );
});

test( 'dlansp: dlansp_3x3_max_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_3x3_max_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlansp( 'max', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assert.strictEqual( typeof result, 'number', 'returns a number' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_3x3_one_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_3x3_one_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlansp( 'one-norm', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_3x3_inf_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_3x3_inf_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlansp( 'inf-norm', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_3x3_frob_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_3x3_frob_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlansp( 'frobenius', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_3x3_max_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_3x3_max_l;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 5.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlansp( 'max', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_3x3_one_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_3x3_one_l;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 5.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlansp( 'one-norm', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_3x3_inf_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_3x3_inf_l;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 5.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlansp( 'inf-norm', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_3x3_frob_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_3x3_frob_l;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 5.0, 2.0, 7.0 ] );
	work = new Float64Array( 3 );
	result = dlansp( 'frobenius', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 4x4 symmetric matrix:

//   A = [  2.0   3.0  -1.0   4.0 ]

//       [  3.0   5.0   2.0  -6.0 ]

//       [ -1.0   2.0   7.0   1.0 ]

//       [  4.0  -6.0   1.0   8.0 ]

// Upper packed: 2, 3, 5, -1, 2, 7, 4, -6, 1, 8

// Lower packed: 2, 3, -1, 4, 5, 2, -6, 7, 1, 8

test( 'dlansp: dlansp_4x4_max_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_4x4_max_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0, 4.0, -6.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlansp( 'max', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_4x4_one_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_4x4_one_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0, 4.0, -6.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlansp( 'one-norm', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_4x4_inf_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_4x4_inf_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0, 4.0, -6.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlansp( 'inf-norm', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_4x4_frob_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_4x4_frob_u;
	ap = new Float64Array( [ 2.0, 3.0, 5.0, -1.0, 2.0, 7.0, 4.0, -6.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlansp( 'frobenius', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_4x4_max_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_4x4_max_l;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 4.0, 5.0, 2.0, -6.0, 7.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlansp( 'max', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_4x4_one_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_4x4_one_l;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 4.0, 5.0, 2.0, -6.0, 7.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlansp( 'one-norm', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_4x4_inf_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_4x4_inf_l;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 4.0, 5.0, 2.0, -6.0, 7.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlansp( 'inf-norm', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_4x4_frob_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_4x4_frob_l;
	ap = new Float64Array( [ 2.0, 3.0, -1.0, 4.0, 5.0, 2.0, -6.0, 7.0, 1.0, 8.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = dlansp( 'frobenius', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=0 quick return
test( 'dlansp: dlansp_n0', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_n0;
	ap = new Float64Array( 0 );
	work = new Float64Array( 0 );
	result = dlansp( 'max', 'upper', 0, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=1 matrix, single element = -5.5
test( 'dlansp: dlansp_1x1_max', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_1x1_max;
	ap = new Float64Array( [ -5.5 ] );
	work = new Float64Array( 1 );
	result = dlansp( 'max', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_1x1_one', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_1x1_one;
	ap = new Float64Array( [ -5.5 ] );
	work = new Float64Array( 1 );
	result = dlansp( 'one-norm', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_1x1_inf', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_1x1_inf;
	ap = new Float64Array( [ -5.5 ] );
	work = new Float64Array( 1 );
	result = dlansp( 'inf-norm', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dlansp: dlansp_1x1_frob', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = dlansp_1x1_frob;
	ap = new Float64Array( [ -5.5 ] );
	work = new Float64Array( 1 );
	result = dlansp( 'frobenius', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
