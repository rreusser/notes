/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var drotmg = require( './../lib/base.js' );

// FIXTURES //

var basic_q1_gt_q2 = require( './fixtures/basic_q1_gt_q2.json' );
var q2_gt_q1 = require( './fixtures/q2_gt_q1.json' );
var dd1_negative = require( './fixtures/dd1_negative.json' );
var dy1_zero = require( './fixtures/dy1_zero.json' );
var dd2_zero = require( './fixtures/dd2_zero.json' );
var q2_negative = require( './fixtures/q2_negative.json' );
var rescale_small = require( './fixtures/rescale_small.json' );
var rescale_large = require( './fixtures/rescale_large.json' );
var dd1_zero = require( './fixtures/dd1_zero.json' );
var dx1_zero = require( './fixtures/dx1_zero.json' );
var negative_dy1 = require( './fixtures/negative_dy1.json' );
var equal_d = require( './fixtures/equal_d.json' );
var rescale_dd2_small = require( './fixtures/rescale_dd2_small.json' );
var rescale_dd2_large = require( './fixtures/rescale_dd2_large.json' );

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
	var delta;
	if ( expected === 0.0 ) {
		delta = Math.abs( actual );
	} else {
		delta = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	}
	assert.ok( delta <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (delta=' + delta + ')' ); // eslint-disable-line max-len
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

test( 'drotmg: basic case |q1| > |q2| (flag=0)', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = basic_q1_gt_q2;
	D = new Float64Array( [ 2.0, 1.0 ] );
	x = new Float64Array( [ 3.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, 4.0, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: |q2| > |q1| (flag=1)', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = q2_gt_q1;
	D = new Float64Array( [ 1.0, 2.0 ] );
	x = new Float64Array( [ 1.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, 3.0, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: dd1 < 0 (error case, zeros everything)', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = dd1_negative;
	D = new Float64Array( [ -1.0, 1.0 ] );
	x = new Float64Array( [ 1.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, 1.0, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: dy1 = 0 (quick return, flag=-2)', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = dy1_zero;
	D = new Float64Array( [ 1.0, 1.0 ] );
	x = new Float64Array( [ 1.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, 0.0, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: dd2 = 0 (p2 = 0, quick return, flag=-2)', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = dd2_zero;
	D = new Float64Array( [ 1.0, 0.0 ] );
	x = new Float64Array( [ 1.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, 1.0, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: q2 < 0 (negative definite, zeros everything)', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = q2_negative;
	D = new Float64Array( [ 1.0, -1.0 ] );
	x = new Float64Array( [ 1.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, 2.0, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: rescaling (very small dd1)', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = rescale_small;
	D = new Float64Array( [ 1.0e-10, 1.0 ] );
	x = new Float64Array( [ 1.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, 1.0, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: rescaling (very large dd1)', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = rescale_large;
	D = new Float64Array( [ 1.0e10, 1.0 ] );
	x = new Float64Array( [ 1.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, 1.0, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: dd1 = 0 (degenerate, q1 = 0)', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = dd1_zero;
	D = new Float64Array( [ 0.0, 3.0 ] );
	x = new Float64Array( [ 5.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, 4.0, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: dx1 = 0 (q1 = 0, swap)', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = dx1_zero;
	D = new Float64Array( [ 2.0, 3.0 ] );
	x = new Float64Array( [ 0.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, 4.0, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: negative dy1', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = negative_dy1;
	D = new Float64Array( [ 2.0, 1.0 ] );
	x = new Float64Array( [ 5.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, -3.0, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: equal d values', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = equal_d;
	D = new Float64Array( [ 1.0, 1.0 ] );
	x = new Float64Array( [ 3.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, 4.0, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: rescaling dd2 very small', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = rescale_dd2_small;
	D = new Float64Array( [ 1.0, 1.0e-10 ] );
	x = new Float64Array( [ 1.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, 1.0e5, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: rescaling dd2 very large', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = rescale_dd2_large;
	D = new Float64Array( [ 1.0, 1.0e10 ] );
	x = new Float64Array( [ 1.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 0, x, 1, 0, 1.0e-5, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 1 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 0 ], tc.dx1, 1e-14, 'dx1' );
	assertArrayClose( dparam, tc.dparam, 1e-14, 'dparam' );
});

test( 'drotmg: supports non-unit strides for D', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = basic_q1_gt_q2;

	// dd1=2, dd2=1 at positions 0 and 2 with stride 2
	D = new Float64Array( [ 2.0, 99.0, 1.0 ] );
	x = new Float64Array( [ 3.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 2, 0, x, 1, 0, 4.0, dparam, 1, 0 );

	assertClose( D[ 0 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 2 ], tc.dd2, 1e-14, 'dd2' );
	assert.equal( D[ 1 ], 99.0, 'gap element unchanged' );
});

test( 'drotmg: supports offsets for D and x1', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = basic_q1_gt_q2;

	// dd1=2 at index 1, dd2=1 at index 2 with stride 1
	D = new Float64Array( [ 99.0, 2.0, 1.0 ] );
	x = new Float64Array( [ 99.0, 3.0 ] );
	dparam = new Float64Array( 5 );

	drotmg( D, 1, 1, x, 1, 1, 4.0, dparam, 1, 0 );

	assertClose( D[ 1 ], tc.dd1, 1e-14, 'dd1' );
	assertClose( D[ 2 ], tc.dd2, 1e-14, 'dd2' );
	assertClose( x[ 1 ], tc.dx1, 1e-14, 'dx1' );
	assert.equal( D[ 0 ], 99.0, 'prefix element unchanged' );
	assert.equal( x[ 0 ], 99.0, 'prefix element unchanged' );
});

test( 'drotmg: supports non-unit stride for param', function t() {
	var dparam;
	var tc;
	var D;
	var x;

	tc = basic_q1_gt_q2;
	D = new Float64Array( [ 2.0, 1.0 ] );
	x = new Float64Array( [ 3.0 ] );
	dparam = new Float64Array( 9 ); // 5 elements with stride 2

	drotmg( D, 1, 0, x, 1, 0, 4.0, dparam, 2, 0 );

	assertClose( dparam[ 0 ], tc.dparam[ 0 ], 1e-14, 'dparam[0]' );
	assertClose( dparam[ 2 ], tc.dparam[ 1 ], 1e-14, 'dparam[1]' );
	assertClose( dparam[ 4 ], tc.dparam[ 2 ], 1e-14, 'dparam[2]' );
	assertClose( dparam[ 6 ], tc.dparam[ 3 ], 1e-14, 'dparam[3]' );
	assertClose( dparam[ 8 ], tc.dparam[ 4 ], 1e-14, 'dparam[4]' );
});
