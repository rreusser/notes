/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*    http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var izamax = require( './../lib' );
var base = require( './../lib/base.js' );

// FIXTURES //

var basic = require( './fixtures/basic.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var stride = require( './fixtures/stride.json' );
var equal = require( './fixtures/equal.json' );
var negative = require( './fixtures/negative.json' );

// TESTS //

test( 'izamax: main export is a function', function t() {
	assert.strictEqual( typeof izamax, 'function' );
});

test( 'izamax: attached to the main export is an `ndarray` method', function t() { // eslint-disable-line max-len
	assert.strictEqual( typeof izamax.ndarray, 'function' );
});

test( 'izamax: basic (n=4, strideX=1)', function t() {
	var result;
	var tc;
	var zx;

	tc = basic;
	zx = new Complex128Array( [ 1, 2, 5, 1, 2, 3, 4, 0 ] );
	result = base( 4, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'izamax: n=0 returns -1', function t() {
	var result;
	var tc;
	var zx;

	tc = n_zero;
	zx = new Complex128Array( [ 1, 2 ] );
	result = base( 0, zx, 1, 0 );
	assert.strictEqual( result, -1 );
	assert.strictEqual( tc.result, 0 );
});

test( 'izamax: n=1 returns 0', function t() {
	var result;
	var tc;
	var zx;

	tc = n_one;
	zx = new Complex128Array( [ 1, 2 ] );
	result = base( 1, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'izamax: non-unit stride (strideX=2)', function t() {
	var result;
	var tc;
	var zx;

	tc = stride;
	zx = new Complex128Array([
		1,
		2,      // element 0
		99,
		99,    // element 1
		2,
		3,      // element 2
		99,
		99,    // element 3
		10,
		10     // element 4
	]);
	result = base( 3, zx, 2, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'izamax: equal magnitudes returns first', function t() {
	var result;
	var tc;
	var zx;

	tc = equal;
	zx = new Complex128Array( [ 3, 2, 1, 4, 5, 0 ] );
	result = base( 3, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'izamax: negative components', function t() {
	var result;
	var tc;
	var zx;

	tc = negative;
	zx = new Complex128Array( [ -3, -4, 1, 1, -2, 5 ] );
	result = base( 3, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'izamax: offset support', function t() {
	var result;
	var zx;

	zx = new Complex128Array( [ 0, 0, 1, 2, 5, 1, 2, 3, 4, 0 ] );
	result = base( 4, zx, 1, 1 );
	assert.strictEqual( result, 1 );
});
