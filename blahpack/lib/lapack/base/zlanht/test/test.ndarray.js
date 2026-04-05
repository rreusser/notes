/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Complex128Array = require( '@stdlib/array/complex128' );
var zlanht = require( './../lib' );
var base = require( './../lib/base.js' );

// FIXTURES //

var max_norm_n1 = require( './fixtures/max_norm_n1.json' );
var one_norm_n1 = require( './fixtures/one_norm_n1.json' );
var inf_norm_n1 = require( './fixtures/inf_norm_n1.json' );
var frob_norm_n1 = require( './fixtures/frob_norm_n1.json' );
var max_norm_n2 = require( './fixtures/max_norm_n2.json' );
var one_norm_n2 = require( './fixtures/one_norm_n2.json' );
var inf_norm_n2 = require( './fixtures/inf_norm_n2.json' );
var frob_norm_n2 = require( './fixtures/frob_norm_n2.json' );
var max_norm_5x5 = require( './fixtures/max_norm_5x5.json' );
var one_norm_o_5x5 = require( './fixtures/one_norm_o_5x5.json' );
var one_norm_1_5x5 = require( './fixtures/one_norm_1_5x5.json' );
var inf_norm_5x5 = require( './fixtures/inf_norm_5x5.json' );
var frob_norm_5x5 = require( './fixtures/frob_norm_5x5.json' );
var frob_norm_e_5x5 = require( './fixtures/frob_norm_e_5x5.json' );
var max_norm_4x4 = require( './fixtures/max_norm_4x4.json' );
var one_norm_4x4 = require( './fixtures/one_norm_4x4.json' );
var inf_norm_4x4 = require( './fixtures/inf_norm_4x4.json' );
var frob_norm_4x4 = require( './fixtures/frob_norm_4x4.json' );

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

test( 'zlanht: main export is a function', function t() {
	assert.strictEqual( typeof zlanht, 'function' );
});

test( 'zlanht: attached to the main export is an `ndarray` method', function t() { // eslint-disable-line max-len
	assert.strictEqual( typeof zlanht.ndarray, 'function' );
});

test( 'zlanht: n_zero', function t() {
	var result;
	var d;
	var e;

	d = new Float64Array( 0 );
	e = new Complex128Array( 0 );
	result = base( 'max', 0, d, 1, 0, e, 1, 0 );
	assert.equal( result, 0.0 );
});

test( 'zlanht: max_norm_n1', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = max_norm_n1;
	d = new Float64Array( [ -3.0 ] );
	e = new Complex128Array( 0 );
	result = base( 'max', 1, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: one_norm_n1', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = one_norm_n1;
	d = new Float64Array( [ -3.0 ] );
	e = new Complex128Array( 0 );
	result = base( 'one-norm', 1, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: inf_norm_n1', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = inf_norm_n1;
	d = new Float64Array( [ -3.0 ] );
	e = new Complex128Array( 0 );
	result = base( 'inf-norm', 1, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: frob_norm_n1', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = frob_norm_n1;
	d = new Float64Array( [ -3.0 ] );
	e = new Complex128Array( 0 );
	result = base( 'frobenius', 1, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: max_norm_n2', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = max_norm_n2;
	d = new Float64Array( [ 3.0, -4.0 ] );
	e = new Complex128Array( [ 1.0, 2.0 ] );
	result = base( 'max', 2, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: one_norm_n2', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = one_norm_n2;
	d = new Float64Array( [ 3.0, -4.0 ] );
	e = new Complex128Array( [ 1.0, 2.0 ] );
	result = base( 'one-norm', 2, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: inf_norm_n2', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = inf_norm_n2;
	d = new Float64Array( [ 3.0, -4.0 ] );
	e = new Complex128Array( [ 1.0, 2.0 ] );
	result = base( 'inf-norm', 2, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: frob_norm_n2', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = frob_norm_n2;
	d = new Float64Array( [ 3.0, -4.0 ] );
	e = new Complex128Array( [ 1.0, 2.0 ] );
	result = base( 'frobenius', 2, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: max_norm_5x5', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = max_norm_5x5;
	d = new Float64Array( [ 2.0, -4.0, 6.0, -1.0, 3.0 ] );
	e = new Complex128Array( [ 1.0, 2.0, -2.0, 3.0, 3.0, -1.0, 5.0, 4.0 ] );
	result = base( 'max', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: one_norm_O alias same as one-norm (5x5)', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = one_norm_o_5x5;
	d = new Float64Array( [ 2.0, -4.0, 6.0, -1.0, 3.0 ] );
	e = new Complex128Array( [ 1.0, 2.0, -2.0, 3.0, 3.0, -1.0, 5.0, 4.0 ] );
	result = base( 'one-norm', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: one_norm_1 5x5', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = one_norm_1_5x5;
	d = new Float64Array( [ 2.0, -4.0, 6.0, -1.0, 3.0 ] );
	e = new Complex128Array( [ 1.0, 2.0, -2.0, 3.0, 3.0, -1.0, 5.0, 4.0 ] );
	result = base( 'one-norm', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: inf_norm_5x5', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = inf_norm_5x5;
	d = new Float64Array( [ 2.0, -4.0, 6.0, -1.0, 3.0 ] );
	e = new Complex128Array( [ 1.0, 2.0, -2.0, 3.0, 3.0, -1.0, 5.0, 4.0 ] );
	result = base( 'inf-norm', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: frob_norm_5x5', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = frob_norm_5x5;
	d = new Float64Array( [ 2.0, -4.0, 6.0, -1.0, 3.0 ] );
	e = new Complex128Array( [ 1.0, 2.0, -2.0, 3.0, 3.0, -1.0, 5.0, 4.0 ] );
	result = base( 'frobenius', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: frob_norm_E alias same as frobenius (5x5)', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = frob_norm_e_5x5;
	d = new Float64Array( [ 2.0, -4.0, 6.0, -1.0, 3.0 ] );
	e = new Complex128Array( [ 1.0, 2.0, -2.0, 3.0, 3.0, -1.0, 5.0, 4.0 ] );
	result = base( 'frobenius', 5, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: max_norm_4x4', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = max_norm_4x4;
	d = new Float64Array( [ 10.0, -20.0, 30.0, -40.0 ] );
	e = new Complex128Array( [ 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 ] );
	result = base( 'max', 4, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: one_norm_4x4', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = one_norm_4x4;
	d = new Float64Array( [ 10.0, -20.0, 30.0, -40.0 ] );
	e = new Complex128Array( [ 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 ] );
	result = base( 'one-norm', 4, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: inf_norm_4x4', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = inf_norm_4x4;
	d = new Float64Array( [ 10.0, -20.0, 30.0, -40.0 ] );
	e = new Complex128Array( [ 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 ] );
	result = base( 'inf-norm', 4, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht: frob_norm_4x4', function t() {
	var result;
	var tc;
	var d;
	var e;

	tc = frob_norm_4x4;
	d = new Float64Array( [ 10.0, -20.0, 30.0, -40.0 ] );
	e = new Complex128Array( [ 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 ] );
	result = base( 'frobenius', 4, d, 1, 0, e, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'zlanht.ndarray: throws on invalid norm', function t() {
	var d;
	var e;

	d = new Float64Array( [ 1.0 ] );
	e = new Complex128Array( 0 );
	assert.throws( function badNorm() {
		zlanht.ndarray( 'bad-norm', 1, d, 1, 0, e, 1, 0 );
	}, /invalid argument/ );
});
