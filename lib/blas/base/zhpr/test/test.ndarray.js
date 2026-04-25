/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhpr = require( './../lib/ndarray.js' );

// FIXTURES //

var upper_basic = require( './fixtures/upper_basic.json' );
var lower_basic = require( './fixtures/lower_basic.json' );
var alpha_zero = require( './fixtures/alpha_zero.json' );
var n_zero = require( './fixtures/n_zero.json' );
var scalar = require( './fixtures/scalar.json' );
var upper_stride_2 = require( './fixtures/upper_stride_2.json' );
var zero_element = require( './fixtures/zero_element.json' );
var lower_stride_2 = require( './fixtures/lower_stride_2.json' );

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
	for ( i = 0; i < expected.length; i += 1 ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}

// TESTS //

test( 'zhpr is a function', function t() {
	assert.strictEqual( typeof zhpr, 'function' );
});

test( 'zhpr: upper_basic (uplo=U, N=3, alpha=2, unit stride)', function t() {
	var result;
	var APv;
	var tc;
	var AP;
	var x;

	tc = upper_basic;
	AP = new Complex128Array([
		2.0,
		0.0,
		1.0,
		1.0,
		4.0,
		0.0,
		3.0,
		-2.0,
		2.0,
		1.0,
		5.0,
		0.0
	]);
	x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	result = zhpr( 'upper', 3, 2.0, x, 1, 0, AP, 1, 0 );
	assert.strictEqual( result, AP );
	APv = reinterpret( AP, 0 );
	assertArrayClose( toArray( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: lower_basic (uplo=L, N=3, alpha=2, unit stride)', function t() {
	var result;
	var APv;
	var tc;
	var AP;
	var x;

	tc = lower_basic;
	AP = new Complex128Array([
		2.0,
		0.0,
		1.0,
		-1.0,
		3.0,
		2.0,
		4.0,
		0.0,
		2.0,
		-1.0,
		5.0,
		0.0
	]);
	x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	result = zhpr( 'lower', 3, 2.0, x, 1, 0, AP, 1, 0 );
	assert.strictEqual( result, AP );
	APv = reinterpret( AP, 0 );
	assertArrayClose( toArray( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: alpha_zero (alpha=0, no-op)', function t() {
	var result;
	var APv;
	var tc;
	var AP;
	var x;

	tc = alpha_zero;
	AP = new Complex128Array([
		2.0,
		0.0,
		1.0,
		1.0,
		4.0,
		0.0,
		3.0,
		-2.0,
		2.0,
		1.0,
		5.0,
		0.0
	]);
	x = new Complex128Array( [ 1.0, 0.5, 2.0, -1.0, 3.0, 1.0 ] );
	result = zhpr( 'upper', 3, 0.0, x, 1, 0, AP, 1, 0 );
	assert.strictEqual( result, AP );
	APv = reinterpret( AP, 0 );
	assertArrayClose( toArray( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: n_zero (N=0 quick return)', function t() {
	var result;
	var APv;
	var tc;
	var AP;
	var x;

	tc = n_zero;
	AP = new Complex128Array( [ 99.0, 0.0 ] );
	x = new Complex128Array( [ 1.0, 0.5 ] );
	result = zhpr( 'upper', 0, 1.0, x, 1, 0, AP, 1, 0 );
	assert.strictEqual( result, AP );
	APv = reinterpret( AP, 0 );
	assertArrayClose( toArray( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: scalar (N=1, alpha=1.5)', function t() {
	var result;
	var APv;
	var tc;
	var AP;
	var x;

	tc = scalar;
	AP = new Complex128Array( [ 3.0, 0.0 ] );
	x = new Complex128Array( [ 2.0, 1.0 ] );
	result = zhpr( 'upper', 1, 1.5, x, 1, 0, AP, 1, 0 );
	assert.strictEqual( result, AP );
	APv = reinterpret( AP, 0 );
	assertArrayClose( toArray( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: upper_stride_2 (uplo=U, N=3, strideX=2)', function t() {
	var result;
	var APv;
	var tc;
	var AP;
	var x;

	tc = upper_stride_2;
	AP = new Complex128Array([
		2.0,
		0.0,
		1.0,
		1.0,
		4.0,
		0.0,
		3.0,
		-2.0,
		2.0,
		1.0,
		5.0,
		0.0
	]);
	x = new Complex128Array([
		1.0,
		0.5,
		0.0,
		0.0,
		2.0,
		-1.0,
		0.0,
		0.0,
		3.0,
		1.0,
		0.0,
		0.0
	]);
	result = zhpr( 'upper', 3, 2.0, x, 2, 0, AP, 1, 0 );
	assert.strictEqual( result, AP );
	APv = reinterpret( AP, 0 );
	assertArrayClose( toArray( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: zero_element (x[1]=0, exercises skip branch)', function t() {
	var result;
	var APv;
	var tc;
	var AP;
	var x;

	tc = zero_element;
	AP = new Complex128Array([
		2.0,
		0.0,
		1.0,
		1.0,
		4.0,
		0.0,
		3.0,
		-2.0,
		2.0,
		1.0,
		5.0,
		0.0
	]);
	x = new Complex128Array( [ 1.0, 0.5, 0.0, 0.0, 3.0, 1.0 ] );
	result = zhpr( 'upper', 3, 2.0, x, 1, 0, AP, 1, 0 );
	assert.strictEqual( result, AP );
	APv = reinterpret( AP, 0 );
	assertArrayClose( toArray( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: lower_stride_2 (uplo=L, N=3, strideX=2)', function t() {
	var result;
	var APv;
	var tc;
	var AP;
	var x;

	tc = lower_stride_2;
	AP = new Complex128Array([
		2.0,
		0.0,
		1.0,
		-1.0,
		3.0,
		2.0,
		4.0,
		0.0,
		2.0,
		-1.0,
		5.0,
		0.0
	]);
	x = new Complex128Array([
		1.0,
		0.5,
		0.0,
		0.0,
		2.0,
		-1.0,
		0.0,
		0.0,
		3.0,
		1.0,
		0.0,
		0.0
	]);
	result = zhpr( 'lower', 3, 2.0, x, 2, 0, AP, 1, 0 );
	assert.strictEqual( result, AP );
	APv = reinterpret( AP, 0 );
	assertArrayClose( toArray( APv ), tc.AP, 1e-14, 'AP' );
});

test( 'zhpr: returns AP', function t() {
	var result;
	var AP;
	var x;

	AP = new Complex128Array( [ 1.0, 0.0 ] );
	x = new Complex128Array( [ 1.0, 0.0 ] );
	result = zhpr( 'upper', 1, 1.0, x, 1, 0, AP, 1, 0 );
	assert.strictEqual( result, AP );
});

test( 'zhpr: all zero x leaves AP unchanged except diagonal imaginary forced to zero', function t() { // eslint-disable-line max-len
	var APv;
	var AP;
	var x;

	AP = new Complex128Array([
		1.0,
		0.5,
		2.0,
		1.0,
		3.0,
		0.7,
		4.0,
		0.3,
		5.0,
		0.2,
		6.0,
		0.9
	]);
	x = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] );
	zhpr( 'upper', 3, 2.0, x, 1, 0, AP, 1, 0 );
	APv = reinterpret( AP, 0 );
	assert.strictEqual( APv[ 0 ], 1.0 );
	assert.strictEqual( APv[ 1 ], 0.0 );
	assert.strictEqual( APv[ 2 ], 2.0 );
	assert.strictEqual( APv[ 3 ], 1.0 );
	assert.strictEqual( APv[ 4 ], 3.0 );
	assert.strictEqual( APv[ 5 ], 0.0 );
	assert.strictEqual( APv[ 6 ], 4.0 );
	assert.strictEqual( APv[ 7 ], 0.3 );
	assert.strictEqual( APv[ 8 ], 5.0 );
	assert.strictEqual( APv[ 9 ], 0.2 );
	assert.strictEqual( APv[ 10 ], 6.0 );
	assert.strictEqual( APv[ 11 ], 0.0 );
});

test( 'zhpr: lower with all zero x forces diagonal imaginary to zero', function t() { // eslint-disable-line max-len
	var APv;
	var AP;
	var x;

	AP = new Complex128Array([
		1.0,
		0.5,
		2.0,
		1.0,
		3.0,
		0.7,
		4.0,
		0.3,
		5.0,
		0.2,
		6.0,
		0.9
	]);
	x = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] );
	zhpr( 'lower', 3, 2.0, x, 1, 0, AP, 1, 0 );
	APv = reinterpret( AP, 0 );
	assert.strictEqual( APv[ 0 ], 1.0 );
	assert.strictEqual( APv[ 1 ], 0.0 );
	assert.strictEqual( APv[ 2 ], 2.0 );
	assert.strictEqual( APv[ 3 ], 1.0 );
	assert.strictEqual( APv[ 4 ], 3.0 );
	assert.strictEqual( APv[ 5 ], 0.7 );
	assert.strictEqual( APv[ 6 ], 4.0 );
	assert.strictEqual( APv[ 7 ], 0.0 );
	assert.strictEqual( APv[ 8 ], 5.0 );
	assert.strictEqual( APv[ 9 ], 0.2 );
	assert.strictEqual( APv[ 10 ], 6.0 );
	assert.strictEqual( APv[ 11 ], 0.0 );
});

test( 'zhpr: offset support for x and AP', function t() {
	var APv;
	var AP;
	var x;

	AP = new Complex128Array([
		999.0,
		999.0,      // padding (1 complex element)
		3.0,
		0.0            // AP(0,0) = (3,0)
	]);
	x = new Complex128Array([
		999.0,
		999.0,       // padding (1 complex element)
		2.0,
		1.0            // x(0) = (2,1)
	]);
	zhpr( 'upper', 1, 1.5, x, 1, 1, AP, 1, 1 );
	APv = reinterpret( AP, 0 );
	assert.strictEqual( APv[ 0 ], 999.0 );
	assert.strictEqual( APv[ 1 ], 999.0 );
	assertClose( APv[ 2 ], 10.5, 1e-14, 'AP[0] real' );
	assert.strictEqual( APv[ 3 ], 0.0 );
});
