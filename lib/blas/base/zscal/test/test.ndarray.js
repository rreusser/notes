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
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zscal = require( './../lib' );
var base = require( './../lib/base.js' );

// FIXTURES //

var zscal_basic = require( './fixtures/zscal_basic.json' );
var zscal_n_zero = require( './fixtures/zscal_n_zero.json' );
var zscal_za_zero = require( './fixtures/zscal_za_zero.json' );
var zscal_stride = require( './fixtures/zscal_stride.json' );
var zscal_za_one = require( './fixtures/zscal_za_one.json' );

// FUNCTIONS //

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= 1e-14, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
}

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
	assert.strictEqual( actual.length, expected.length, msg + ': length mismatch' ); // eslint-disable-line max-len
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], msg + '[' + i + ']' );
	}
}

// FUNCTIONS //

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

test( 'zscal: main export is a function', function t() {
	assert.strictEqual( typeof zscal, 'function' );
});

test( 'zscal: attached to the main export is an `ndarray` method', function t() { // eslint-disable-line max-len
	assert.strictEqual( typeof zscal.ndarray, 'function' );
});

test( 'zscal: basic scale (N=3, za=(2,3), strideX=1)', function t() {
	var result;
	var tc;
	var za;
	var zx;

	tc = zscal_basic;
	za = new Complex128( 2.0, 3.0 );
	zx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	result = base( 3, za, zx, 1, 0 );
	assert.strictEqual( result, zx );
	assertArrayClose( toArray( reinterpret( zx, 0 ) ), tc.zx, 'zscal_basic' );
});

test( 'zscal: N=0 is a no-op', function t() {
	var result;
	var tc;
	var za;
	var zx;

	tc = zscal_n_zero;
	za = new Complex128( 5.0, 6.0 );
	zx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	result = base( 0, za, zx, 1, 0 );
	assert.strictEqual( result, zx );
	assertArrayClose( toArray( reinterpret( zx, 0 ) ), tc.zx, 'zscal_n_zero' );
});

test( 'zscal: za=(0,0) zeros out vector', function t() {
	var result;
	var tc;
	var za;
	var zx;

	tc = zscal_za_zero;
	za = new Complex128( 0.0, 0.0 );
	zx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	result = base( 3, za, zx, 1, 0 );
	assert.strictEqual( result, zx );
	assertArrayClose( toArray( reinterpret( zx, 0 ) ), tc.zx, 'zscal_za_zero' );
});

test( 'zscal: non-unit stride (strideX=2, za=(0,1))', function t() {
	var result;
	var tc;
	var za;
	var zx;

	tc = zscal_stride;
	za = new Complex128( 0.0, 1.0 );
	zx = new Complex128Array([
		1.0, 2.0, 99.0, 99.0, 3.0, 4.0, 99.0, 99.0, 5.0, 6.0
	]);
	result = base( 3, za, zx, 2, 0 );
	assert.strictEqual( result, zx );
	assertArrayClose( toArray( reinterpret( zx, 0 ) ), tc.zx, 'zscal_stride' );
});

test( 'zscal: za=(1,0) is identity (no-op)', function t() {
	var result;
	var tc;
	var za;
	var zx;

	tc = zscal_za_one;
	za = new Complex128( 1.0, 0.0 );
	zx = new Complex128Array( [ 7.0, 8.0, 9.0, 10.0 ] );
	result = base( 2, za, zx, 1, 0 );
	assert.strictEqual( result, zx );
	assertArrayClose( toArray( reinterpret( zx, 0 ) ), tc.zx, 'zscal_za_one' );
});

test( 'zscal: negative N is a no-op', function t() {
	var za;
	var zx;
	var xv;

	za = new Complex128( 2.0, 3.0 );
	zx = new Complex128Array( [ 1.0, 2.0 ] );
	xv = reinterpret( zx, 0 );
	base( -1, za, zx, 1, 0 );
	assert.strictEqual( xv[ 0 ], 1.0 );
	assert.strictEqual( xv[ 1 ], 2.0 );
});
