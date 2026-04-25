/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var izmax1 = require( './../lib/ndarray.js' );

// FIXTURES //

var basic_3 = require( './fixtures/basic_3.json' );
var max_first = require( './fixtures/max_first.json' );
var max_last = require( './fixtures/max_last.json' );
var n_one = require( './fixtures/n_one.json' );
var stride2 = require( './fixtures/stride2.json' );
var equal_magnitudes = require( './fixtures/equal_magnitudes.json' );

// TESTS //

test( 'izmax1: main export is a function', function t() {
	assert.strictEqual( typeof izmax1, 'function' );
});

test( 'izmax1: basic 3-element vector', function t() {
	var result;
	var tc;
	var zx;

	tc = basic_3;
	zx = new Complex128Array( [ 1, 0, 3, 4, 2, 0 ] );
	result = izmax1( 3, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1, 'result (0-based)' );
});

test( 'izmax1: max at first element', function t() {
	var result;
	var tc;
	var zx;

	tc = max_first;
	zx = new Complex128Array( [ 5, 12, 3, 4, 1, 0 ] );
	result = izmax1( 3, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1, 'result (0-based)' );
});

test( 'izmax1: max at last element', function t() {
	var result;
	var tc;
	var zx;

	tc = max_last;
	zx = new Complex128Array( [ 1, 0, 2, 0, 3, 0, 0, 10 ] );
	result = izmax1( 4, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1, 'result (0-based)' );
});

test( 'izmax1: N=1', function t() {
	var result;
	var tc;
	var zx;

	tc = n_one;
	zx = new Complex128Array( [ 7, 3 ] );
	result = izmax1( 1, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1, 'result (0-based)' );
});

test( 'izmax1: stride=2', function t() {
	var result;
	var tc;
	var zx;

	tc = stride2;
	zx = new Complex128Array( [ 1, 0, 99, 99, 3, 4, 99, 99, 2, 0 ] );
	result = izmax1( 3, zx, 2, 0 );
	assert.strictEqual( result, tc.result - 1, 'result (0-based)' );
});

test( 'izmax1: all equal magnitudes returns first', function t() {
	var result;
	var tc;
	var zx;

	tc = equal_magnitudes;
	zx = new Complex128Array( [ 3, 4, 0, 5, 5, 0 ] );
	result = izmax1( 3, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1, 'result (0-based)' );
});

test( 'izmax1: N < 1 returns -1', function t() {
	var result;
	var zx;

	zx = new Complex128Array( [ 1, 0 ] );
	result = izmax1( 0, zx, 1, 0 );
	assert.strictEqual( result, -1 );
});

test( 'izmax1: nonzero offset', function t() {
	var result;
	var zx;

	zx = new Complex128Array( [ 99, 99, 1, 0, 3, 4, 2, 0 ] );
	result = izmax1( 3, zx, 1, 1 );
	assert.strictEqual( result, 1 );
});
