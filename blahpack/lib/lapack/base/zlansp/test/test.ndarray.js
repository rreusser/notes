/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zlansp = require( './../lib/base.js' );

// FIXTURES //

var zlansp_3x3_max_u = require( './fixtures/zlansp_3x3_max_u.json' );
var zlansp_3x3_one_u = require( './fixtures/zlansp_3x3_one_u.json' );
var zlansp_3x3_inf_u = require( './fixtures/zlansp_3x3_inf_u.json' );
var zlansp_3x3_frob_u = require( './fixtures/zlansp_3x3_frob_u.json' );
var zlansp_3x3_max_l = require( './fixtures/zlansp_3x3_max_l.json' );
var zlansp_3x3_one_l = require( './fixtures/zlansp_3x3_one_l.json' );
var zlansp_3x3_inf_l = require( './fixtures/zlansp_3x3_inf_l.json' );
var zlansp_3x3_frob_l = require( './fixtures/zlansp_3x3_frob_l.json' );
var zlansp_4x4_max_u = require( './fixtures/zlansp_4x4_max_u.json' );
var zlansp_4x4_one_u = require( './fixtures/zlansp_4x4_one_u.json' );
var zlansp_4x4_inf_u = require( './fixtures/zlansp_4x4_inf_u.json' );
var zlansp_4x4_frob_u = require( './fixtures/zlansp_4x4_frob_u.json' );
var zlansp_4x4_max_l = require( './fixtures/zlansp_4x4_max_l.json' );
var zlansp_4x4_one_l = require( './fixtures/zlansp_4x4_one_l.json' );
var zlansp_4x4_inf_l = require( './fixtures/zlansp_4x4_inf_l.json' );
var zlansp_4x4_frob_l = require( './fixtures/zlansp_4x4_frob_l.json' );
var zlansp_n0 = require( './fixtures/zlansp_n0.json' );
var zlansp_1x1_max = require( './fixtures/zlansp_1x1_max.json' );
var zlansp_1x1_one = require( './fixtures/zlansp_1x1_one.json' );
var zlansp_1x1_inf = require( './fixtures/zlansp_1x1_inf.json' );
var zlansp_1x1_frob = require( './fixtures/zlansp_1x1_frob.json' );

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

// 3x3 complex symmetric matrix:
//   A = [ (2,1)   (1,2)   (3,-1) ]
//       [ (1,2)   (5,-1)  (2,1)  ]
//       [ (3,-1)  (2,1)   (4,2)  ]

// Upper packed (col-major): (2,1),(1,2),(5,-1),(3,-1),(2,1),(4,2)
// Lower packed (col-major): (2,1),(1,2),(3,-1),(5,-1),(2,1),(4,2)

test( 'zlansp is a function', function t() {
	assert.strictEqual( typeof zlansp, 'function' );
});

test( 'zlansp: zlansp_3x3_max_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_3x3_max_u;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'max', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assert.strictEqual( typeof result, 'number', 'returns a number' );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_one_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_3x3_one_u;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'one-norm', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_inf_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_3x3_inf_u;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'inf-norm', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_frob_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_3x3_frob_u;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'frobenius', 'upper', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_max_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_3x3_max_l;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 5.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'max', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_one_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_3x3_one_l;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 5.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'one-norm', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_inf_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_3x3_inf_l;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 5.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'inf-norm', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_3x3_frob_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_3x3_frob_l;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 5.0, -1.0, 2.0, 1.0, 4.0, 2.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 3 );
	result = zlansp( 'frobenius', 'lower', 3, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// 4x4 complex symmetric matrix:

//   A = [ (2,1)      (1,2)    (3,-1)   (0.5,0.5) ]

//       [ (1,2)      (5,-1)   (2,1)    (1,-2)    ]

//       [ (3,-1)     (2,1)    (4,2)    (3,0)     ]

//       [ (0.5,0.5)  (1,-2)   (3,0)    (6,-3)    ]

// Upper packed: (2,1),(1,2),(5,-1),(3,-1),(2,1),(4,2),(0.5,0.5),(1,-2),(3,0),(6,-3) // eslint-disable-line max-len

// Lower packed: (2,1),(1,2),(3,-1),(0.5,0.5),(5,-1),(2,1),(1,-2),(4,2),(3,0),(6,-3) // eslint-disable-line max-len

test( 'zlansp: zlansp_4x4_max_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_4x4_max_u;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0, 0.5, 0.5, 1.0, -2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'max', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_one_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_4x4_one_u;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0, 0.5, 0.5, 1.0, -2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'one-norm', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_inf_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_4x4_inf_u;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0, 0.5, 0.5, 1.0, -2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'inf-norm', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_frob_U', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_4x4_frob_u;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 5.0, -1.0, 3.0, -1.0, 2.0, 1.0, 4.0, 2.0, 0.5, 0.5, 1.0, -2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'frobenius', 'upper', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_max_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_4x4_max_l;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 0.5, 0.5, 5.0, -1.0, 2.0, 1.0, 1.0, -2.0, 4.0, 2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'max', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_one_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_4x4_one_l;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 0.5, 0.5, 5.0, -1.0, 2.0, 1.0, 1.0, -2.0, 4.0, 2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'one-norm', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_inf_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_4x4_inf_l;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 0.5, 0.5, 5.0, -1.0, 2.0, 1.0, 1.0, -2.0, 4.0, 2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'inf-norm', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_4x4_frob_L', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_4x4_frob_l;
	ap = new Complex128Array( [ 2.0, 1.0, 1.0, 2.0, 3.0, -1.0, 0.5, 0.5, 5.0, -1.0, 2.0, 1.0, 1.0, -2.0, 4.0, 2.0, 3.0, 0.0, 6.0, -3.0 ] ); // eslint-disable-line max-len
	work = new Float64Array( 4 );
	result = zlansp( 'frobenius', 'lower', 4, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=0 quick return
test( 'zlansp: zlansp_n0', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_n0;
	ap = new Complex128Array( 0 );
	work = new Float64Array( 0 );
	result = zlansp( 'max', 'upper', 0, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

// N=1 matrix, single element = (3.0, 4.0) => |z| = 5.0
test( 'zlansp: zlansp_1x1_max', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_1x1_max;
	ap = new Complex128Array( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlansp( 'max', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_1x1_one', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_1x1_one;
	ap = new Complex128Array( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlansp( 'one-norm', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_1x1_inf', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_1x1_inf;
	ap = new Complex128Array( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlansp( 'inf-norm', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlansp: zlansp_1x1_frob', function t() {
	var result;
	var work;
	var tc;
	var ap;

	tc = zlansp_1x1_frob;
	ap = new Complex128Array( [ 3.0, 4.0 ] );
	work = new Float64Array( 1 );
	result = zlansp( 'frobenius', 'upper', 1, ap, 1, 0, work, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
