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
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlaswp = require( './../lib' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dlaswp.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name;
	} );
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
	var tc = findCase( 'basic_forward' );
	var a = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	dlaswp.ndarray( 2, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	assertArrayClose( a, tc.a, 'basic_forward' );
});

test( 'dlaswp.ndarray is a no-op when ipiv(k) == k', function t() {
	var ipiv = new Int32Array( [ 0, 1 ] );
	var tc = findCase( 'no_swap' );
	var a = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	dlaswp.ndarray( 2, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	assertArrayClose( a, tc.a, 'no_swap' );
});

test( 'dlaswp.ndarray performs reverse row interchanges (incx=-1)', function t() { // eslint-disable-line max-len
	var ipiv = new Int32Array( [ 2, 1 ] );
	var tc = findCase( 'reverse_pivots' );
	var a = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	dlaswp.ndarray( 2, a, 1, 3, 0, 1, 0, ipiv, 1, 0, -1 );
	assertArrayClose( a, tc.a, 'reverse_pivots' );
});

test( 'dlaswp.ndarray is a no-op when incx=0', function t() {
	var ipiv = new Int32Array( [ 2 ] );
	var tc = findCase( 'incx_zero' );
	var a = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	dlaswp.ndarray( 2, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 0 );
	assertArrayClose( a, tc.a, 'incx_zero' );
});

test( 'dlaswp.ndarray applies sequential swaps', function t() {
	var ipiv = new Int32Array( [ 1, 2 ] );
	var tc = findCase( 'two_swaps' );
	var a = new Float64Array( [ 10, 20, 30 ] );
	dlaswp.ndarray( 1, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	assertArrayClose( a, tc.a, 'two_swaps' );
});

test( 'dlaswp.ndarray exercises block-tiled path (N=40 > 32 columns)', function t() { // eslint-disable-line max-len
	var ipiv;
	var tc;
	var a;
	var i;

	tc = findCase( 'block_tiled' );
	a = new Float64Array( 120 );
	for ( i = 0; i < 120; i++ ) {
		a[ i ] = i + 1;
	}
	ipiv = new Int32Array( [ 2, 1 ] );
	dlaswp.ndarray( 40, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	assertArrayClose( a, tc.a, 'block_tiled' );
});
