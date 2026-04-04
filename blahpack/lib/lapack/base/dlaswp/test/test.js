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
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlaswp = require( './../lib' );

// FIXTURES //

var basic_forward = require( './fixtures/basic_forward.json' );
var no_swap = require( './fixtures/no_swap.json' );
var reverse_pivots = require( './fixtures/reverse_pivots.json' );
var incx_zero = require( './fixtures/incx_zero.json' );
var two_swaps = require( './fixtures/two_swaps.json' );
var block_tiled = require( './fixtures/block_tiled.json' );

// FUNCTIONS //

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assert.equal( actual[i], expected[i], msg + '[' + i + ']' );
	}
}

// TESTS //

// Note: base.js uses 0-based k1/k2 and 0-based IPIV values.
// Fortran uses 1-based, so we convert in the test inputs.

test( 'dlaswp.ndarray performs forward row interchanges', function t() {
	var ipiv = new Int32Array( [ 2, 1 ] );
	var tc = basic_forward;
	var a = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	dlaswp.ndarray( 2, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	assertArrayClose( a, tc.a, 'basic_forward' );
});

test( 'dlaswp.ndarray is a no-op when ipiv(k) == k', function t() {
	var ipiv = new Int32Array( [ 0, 1 ] );
	var tc = no_swap;
	var a = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	dlaswp.ndarray( 2, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	assertArrayClose( a, tc.a, 'no_swap' );
});

test( 'dlaswp.ndarray performs reverse row interchanges (incx=-1)', function t() { // eslint-disable-line max-len
	var ipiv = new Int32Array( [ 2, 1 ] );
	var tc = reverse_pivots;
	var a = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	dlaswp.ndarray( 2, a, 1, 3, 0, 1, 0, ipiv, 1, 0, -1 );
	assertArrayClose( a, tc.a, 'reverse_pivots' );
});

test( 'dlaswp.ndarray is a no-op when incx=0', function t() {
	var ipiv = new Int32Array( [ 2 ] );
	var tc = incx_zero;
	var a = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	dlaswp.ndarray( 2, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 0 );
	assertArrayClose( a, tc.a, 'incx_zero' );
});

test( 'dlaswp.ndarray applies sequential swaps', function t() {
	var ipiv = new Int32Array( [ 1, 2 ] );
	var tc = two_swaps;
	var a = new Float64Array( [ 10, 20, 30 ] );
	dlaswp.ndarray( 1, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	assertArrayClose( a, tc.a, 'two_swaps' );
});

test( 'dlaswp.ndarray exercises block-tiled path (N=40 > 32 columns)', function t() { // eslint-disable-line max-len
	var ipiv;
	var tc;
	var a;
	var i;

	tc = block_tiled;
	a = new Float64Array( 120 );
	for ( i = 0; i < 120; i++ ) {
		a[ i ] = i + 1;
	}
	ipiv = new Int32Array( [ 2, 1 ] );
	dlaswp.ndarray( 40, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	assertArrayClose( a, tc.a, 'block_tiled' );
});
