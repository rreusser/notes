/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var idamax = require( './../lib/ndarray.js' );

// FIXTURES //

var basic = require( './fixtures/basic.json' );
var negative = require( './fixtures/negative.json' );
var stride = require( './fixtures/stride.json' );
var first_max = require( './fixtures/first_max.json' );
var last_max = require( './fixtures/last_max.json' );

// TESTS //

test( 'idamax: main export is a function', function t() {
	assert.strictEqual( typeof idamax, 'function' );
});

test( 'idamax: basic (N=5, stride=1)', function t() {
	var result;
	var tc;
	var x;

	tc = basic;
	x = new Float64Array( [ 1.0, -3.0, 2.0, 5.0, -4.0 ] );
	result = idamax( 5, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: n_zero returns -1', function t() {
	var result;
	var x;

	x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	result = idamax( 0, x, 1, 0 );
	assert.strictEqual( result, -1 );
});

test( 'idamax: n_one returns 0', function t() {
	var result;
	var x;

	x = new Float64Array( [ 42.0 ] );
	result = idamax( 1, x, 1, 0 );
	assert.strictEqual( result, 0 );
});

test( 'idamax: negative values (N=4, stride=1)', function t() {
	var result;
	var tc;
	var x;

	tc = negative;
	x = new Float64Array( [ -2.0, -7.0, -1.0, -3.0 ] );
	result = idamax( 4, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: non-unit stride (N=3, stride=2)', function t() {
	var result;
	var tc;
	var x;

	tc = stride;
	x = new Float64Array( [ 1.0, 99.0, 10.0, 99.0, 2.0 ] );
	result = idamax( 3, x, 2, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: first element is max', function t() {
	var result;
	var tc;
	var x;

	tc = first_max;
	x = new Float64Array( [ 100.0, 1.0, 2.0 ] );
	result = idamax( 3, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: last element is max', function t() {
	var result;
	var tc;
	var x;

	tc = last_max;
	x = new Float64Array( [ 1.0, 2.0, 100.0 ] );
	result = idamax( 3, x, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'idamax: throws RangeError for N<0', function t() {
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	assert.throws( function() {
		idamax( -1, x, 1, 0 );
	}, RangeError );
});

test( 'idamax: stride <= 0 returns -1', function t() {
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	assert.strictEqual( idamax( 3, x, 0, 0 ), -1 );
	assert.strictEqual( idamax( 3, x, -1, 0 ), -1 );
});

test( 'idamax: offset parameter works', function t() {
	var result;
	var x;

	x = new Float64Array( [ 99.0, 1.0, 10.0, 2.0 ] );
	result = idamax( 3, x, 1, 1 );
	assert.strictEqual( result, 1 );
});

test( 'idamax: all equal elements returns 0', function t() {
	var result;
	var x;

	x = new Float64Array( [ 5.0, 5.0, 5.0, 5.0 ] );
	result = idamax( 4, x, 1, 0 );
	assert.strictEqual( result, 0 );
});
