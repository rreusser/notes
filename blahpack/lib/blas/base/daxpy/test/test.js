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

// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync; // eslint-disable-line node/no-sync
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var daxpy = require( './../lib' );
var base = require( './../lib/base.js' );


// VARIABLES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'daxpy.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line max-len
var fixture = lines.map( JSON.parse );


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

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {number} actual - actual value
* @param {number} expected - expected value
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, msg ) {
	var relErr;
	var denom;

	denom = Math.max( Math.abs( expected ), 1.0 );
	relErr = Math.abs( actual - expected ) / denom;
	assert.ok( relErr <= 1e-14, msg + ': expected ' + expected + ', got ' + actual ); // eslint-disable-line max-len
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {Array} actual - actual array
* @param {Array} expected - expected array
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, msg ) {
	var i;
	assert.strictEqual( actual.length, expected.length, msg + ': length mismatch' ); // eslint-disable-line max-len
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], msg + '[' + i + ']' );
	}
}

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {Object} test case
*/
function tc( name ) {
	var i;
	for ( i = 0; i < fixture.length; i++ ) {
		if ( fixture[ i ].name === name ) {
			return fixture[ i ];
		}
	}
	return null;
}


// TESTS //

test( 'daxpy: main export is a function', function t() {
	assert.strictEqual( typeof daxpy, 'function' );
});

test( 'daxpy: attached to the main export is an `ndarray` method', function t() { // eslint-disable-line max-len
	assert.strictEqual( typeof daxpy.ndarray, 'function' );
});

test( 'daxpy: basic (N=5, alpha=2)', function t() {
	var expected;
	var result;
	var x;
	var y;

	expected = tc( 'basic' );
	x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	y = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	result = base( 5, 2.0, x, 1, 0, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( toArray( y ), expected.dy, 'basic' );
});

test( 'daxpy: alpha=0 is a no-op', function t() {
	var expected;
	var result;
	var x;
	var y;

	expected = tc( 'da_zero' );
	x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	y = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	result = base( 5, 0.0, x, 1, 0, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( toArray( y ), expected.dy, 'da_zero' );
});

test( 'daxpy: N=0 is a no-op', function t() {
	var expected;
	var result;
	var x;
	var y;

	expected = tc( 'n_zero' );
	x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	y = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	result = base( 0, 2.0, x, 1, 0, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( toArray( y ), expected.dy, 'n_zero' );
});

test( 'daxpy: N=1', function t() {
	var expected;
	var result;
	var x;
	var y;

	expected = tc( 'n_one' );
	x = new Float64Array( [ 3.0 ] );
	y = new Float64Array( [ 7.0 ] );
	result = base( 1, 5.0, x, 1, 0, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( toArray( y ), expected.dy, 'n_one' );
});

test( 'daxpy: non-unit strides (incx=2, incy=3)', function t() {
	var expected;
	var result;
	var x;
	var y;

	expected = tc( 'stride' );
	x = new Float64Array( [ 1.0, 0.0, 2.0, 0.0, 3.0 ] );
	y = new Float64Array( [ 10.0, 0.0, 0.0, 20.0, 0.0, 0.0, 30.0, 0.0, 0.0 ] ); // eslint-disable-line max-len
	result = base( 3, 2.0, x, 2, 0, y, 3, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( toArray( y ), expected.dy, 'stride' );
});

test( 'daxpy: negative incx', function t() {
	var expected;
	var result;
	var x;
	var y;

	expected = tc( 'neg_incx' );
	x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	y = new Float64Array( [ 10.0, 20.0, 30.0 ] );
	result = base( 3, 1.0, x, -1, 2, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( toArray( y ), expected.dy, 'neg_incx' );
});

test( 'daxpy: unrolled loop (N=10, alpha=3)', function t() {
	var expected;
	var result;
	var x;
	var y;

	expected = tc( 'unrolled' );
	x = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
	y = new Float64Array( [ 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 ] );
	result = base( 10, 3.0, x, 1, 0, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( toArray( y ), expected.dy, 'unrolled' );
});
