/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var real = require( '@stdlib/complex/float64/real' );
var imag = require( '@stdlib/complex/float64/imag' );
var zdotc = require( './../lib/base.js' );

// FIXTURES //

var basic = require( './fixtures/basic.json' );
var n_one = require( './fixtures/n_one.json' );
var conjugation = require( './fixtures/conjugation.json' );
var non_unit_stride = require( './fixtures/non_unit_stride.json' );
var negative_stride = require( './fixtures/negative_stride.json' );
var both_negative = require( './fixtures/both_negative.json' );
var purely_imaginary = require( './fixtures/purely_imaginary.json' );
var larger_n = require( './fixtures/larger_n.json' );

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
* Asserts that a Complex128 result matches expected [real, imag] from fixture.
*/
function assertComplexClose( result, expected, tol, msg ) {
	assertClose( real( result ), expected[ 0 ], tol, msg + ' real' );
	assertClose( imag( result ), expected[ 1 ], tol, msg + ' imag' );
}

// TESTS //

test( 'zdotc: main export is a function', function t() {
	assert.strictEqual( typeof zdotc, 'function' );
});

test( 'zdotc: basic (N=3, unit stride)', function t() {
	var result;
	var tc;
	var x;
	var y;

	tc = basic;
	x = new Complex128Array( tc.x );
	y = new Complex128Array( tc.y );
	result = zdotc( tc.N, x, 1, 0, y, 1, 0 );
	assertComplexClose( result, tc.result, 1e-14, 'basic' );
});

test( 'zdotc: N=0 returns (0,0)', function t() {
	var result;
	var x;
	var y;

	x = new Complex128Array( [ 1, 2, 3, 4 ] );
	y = new Complex128Array( [ 5, 6, 7, 8 ] );
	result = zdotc( 0, x, 1, 0, y, 1, 0 );
	assert.strictEqual( real( result ), 0.0 );
	assert.strictEqual( imag( result ), 0.0 );
});

test( 'zdotc: N<0 returns (0,0)', function t() {
	var result;
	var x;
	var y;

	x = new Complex128Array( [ 1, 2, 3, 4 ] );
	y = new Complex128Array( [ 5, 6, 7, 8 ] );
	result = zdotc( -1, x, 1, 0, y, 1, 0 );
	assert.strictEqual( real( result ), 0.0 );
	assert.strictEqual( imag( result ), 0.0 );
});

test( 'zdotc: N=1', function t() {
	var result;
	var tc;
	var x;
	var y;

	tc = n_one;
	x = new Complex128Array( tc.x );
	y = new Complex128Array( tc.y );
	result = zdotc( tc.N, x, 1, 0, y, 1, 0 );
	assertComplexClose( result, tc.result, 1e-14, 'n_one' );
});

test( 'zdotc: conjugation verification', function t() {
	var result;
	var tc;
	var x;
	var y;

	tc = conjugation;
	x = new Complex128Array( tc.x );
	y = new Complex128Array( tc.y );
	result = zdotc( tc.N, x, 1, 0, y, 1, 0 );
	assertComplexClose( result, tc.result, 1e-14, 'conjugation' );
});

test( 'zdotc: non-unit stride (strideX=2, strideY=1)', function t() {
	var result;
	var tc;
	var x;
	var y;

	tc = non_unit_stride;
	x = new Complex128Array( tc.x );
	y = new Complex128Array( tc.y );
	result = zdotc( tc.N, x, 2, 0, y, 1, 0 );
	assertComplexClose( result, tc.result, 1e-14, 'non_unit_stride' );
});

test( 'zdotc: negative stride (strideX=-1)', function t() {
	var result;
	var tc;
	var x;
	var y;

	tc = negative_stride;
	x = new Complex128Array( tc.x );
	y = new Complex128Array( tc.y );
	result = zdotc( tc.N, x, -1, 2, y, 1, 0 );
	assertComplexClose( result, tc.result, 1e-14, 'negative_stride' );
});

test( 'zdotc: both negative strides', function t() {
	var result;
	var tc;
	var x;
	var y;

	tc = both_negative;
	x = new Complex128Array( tc.x );
	y = new Complex128Array( tc.y );
	result = zdotc( tc.N, x, -1, 2, y, -1, 2 );
	assertComplexClose( result, tc.result, 1e-14, 'both_negative' );
});

test( 'zdotc: purely imaginary vectors', function t() {
	var result;
	var tc;
	var x;
	var y;

	tc = purely_imaginary;
	x = new Complex128Array( tc.x );
	y = new Complex128Array( tc.y );
	result = zdotc( tc.N, x, 1, 0, y, 1, 0 );
	assertComplexClose( result, tc.result, 1e-14, 'purely_imaginary' );
});

test( 'zdotc: larger N (N=6)', function t() {
	var result;
	var tc;
	var x;
	var y;

	tc = larger_n;
	x = new Complex128Array( tc.x );
	y = new Complex128Array( tc.y );
	result = zdotc( tc.N, x, 1, 0, y, 1, 0 );
	assertComplexClose( result, tc.result, 1e-14, 'larger_n' );
});

test( 'zdotc: offsetX and offsetY', function t() {
	var result;
	var x;
	var y;

	x = new Complex128Array( [ 99, 99, 1, 2, 3, 4, 5, 6 ] );
	y = new Complex128Array( [ 99, 99, 99, 99, 7, 8, 9, 10, 11, 12 ] );
	result = zdotc( 3, x, 1, 1, y, 1, 2 );
	assertClose( real( result ), 217.0, 1e-14, 'offset real' );
	assertClose( imag( result ), -18.0, 1e-14, 'offset imag' );
});

test( 'zdotc: self dot product gives squared norm', function t() {
	var result;
	var x;

	x = new Complex128Array( [ 1, 2, 3, 4 ] );
	result = zdotc( 2, x, 1, 0, x, 1, 0 );
	assertClose( real( result ), 30.0, 1e-14, 'self-dot real' );
	assertClose( imag( result ), 0.0, 1e-14, 'self-dot imag' );
});
