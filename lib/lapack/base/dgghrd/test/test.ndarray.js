/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-lines */

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
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgghrd = require( './../lib/ndarray.js' );

// VARIABLES //

// FIXTURES //

var basic_4x4 = require( './fixtures/basic_4x4.json' );
var n_equals_1 = require( './fixtures/n_equals_1.json' );
var no_qz_3x3 = require( './fixtures/no_qz_3x3.json' );
var partial_5x5 = require( './fixtures/partial_5x5.json' );
var accumulate_3x3 = require( './fixtures/accumulate_3x3.json' );
var ilo_eq_ihi = require( './fixtures/ilo_eq_ihi.json' );
var empty_range = require( './fixtures/empty_range.json' );

// FUNCTIONS //

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {Array} actual - actual values
* @param {Array} expected - expected values
* @param {number} tol - relative tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.strictEqual( actual.length, expected.length, msg + ': length mismatch (' + actual.length + ' vs ' + expected.length + ')' ); // eslint-disable-line max-len
	for ( i = 0; i < expected.length; i += 1 ) {
		if ( expected[ i ] === 0.0 ) {
			assert.ok( Math.abs( actual[ i ] ) <= 1e-14, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
		} else {
			relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 ); // eslint-disable-line max-len
			assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] ); // eslint-disable-line max-len
		}
	}
}

/**
* Converts a Float64Array to a plain Array.
*
* @private
* @param {Float64Array} arr - typed array
* @returns {Array} plain array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i += 1 ) {
		out.push( arr[ i ] );
	}
	return out;
}

/**
* Extracts an N-by-N submatrix stored column-major (stride1=1, stride2=N).
*
* @private
* @param {Float64Array} M - flat matrix array
* @param {integer} N - matrix order
* @returns {Array} N*N array in column-major order
*/
function extractMatrix( M, N ) {
	var out = [];
	var i;
	for ( i = 0; i < N * N; i += 1 ) {
		out.push( M[ i ] );
	}
	return out;
}

// TESTS //

test( 'dgghrd is a function', function t() {
	assert.strictEqual( typeof dgghrd, 'function' );
});

test( 'dgghrd: basic_4x4 (COMPQ=initialize, COMPZ=initialize)', function t() {
	var info;
	var tc;
	var A;
	var B;
	var Q;
	var Z;
	var n;

	tc = basic_4x4;
	n = 4;
	A = new Float64Array([
		2.0,
		1.0,
		0.5,
		0.25,
		1.0,
		3.0,
		2.0,
		1.0,
		0.5,
		1.0,
		4.0,
		0.5,
		1.0,
		0.5,
		1.0,
		2.0
	]);
	B = new Float64Array([
		3.0,
		0.0,
		0.0,
		0.0,
		1.0,
		2.0,
		0.0,
		0.0,
		0.5,
		1.0,
		4.0,
		0.0,
		0.25,
		0.5,
		1.0,
		1.0
	]);
	Q = new Float64Array( n * n );
	Z = new Float64Array( n * n );
	info = dgghrd( 'initialize', 'initialize', n, 1, 4, A, 1, n, 0, B, 1, n, 0, Q, 1, n, 0, Z, 1, n, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0, 'info = 0' );
	assertArrayClose( extractMatrix( A, n ), tc.A, 1e-14, 'A' );
	assertArrayClose( extractMatrix( B, n ), tc.B, 1e-14, 'B' );
	assertArrayClose( extractMatrix( Q, n ), tc.Q, 1e-14, 'Q' );
	assertArrayClose( extractMatrix( Z, n ), tc.Z, 1e-14, 'Z' );
});

test( 'dgghrd: n_equals_1 (quick return)', function t() {
	var info;
	var tc;
	var A;
	var B;
	var Q;
	var Z;

	tc = n_equals_1;
	A = new Float64Array([ 5.0 ]);
	B = new Float64Array([ 2.0 ]);
	Q = new Float64Array( 1 );
	Z = new Float64Array( 1 );
	info = dgghrd( 'initialize', 'initialize', 1, 1, 1, A, 1, 1, 0, B, 1, 1, 0, Q, 1, 1, 0, Z, 1, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0, 'info = 0' );
	assertArrayClose( toArray( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( toArray( B ), tc.B, 1e-14, 'B' );
	assertArrayClose( toArray( Q ), tc.Q, 1e-14, 'Q' );
	assertArrayClose( toArray( Z ), tc.Z, 1e-14, 'Z' );
});

test( 'dgghrd: no_qz_3x3 (COMPQ=none, COMPZ=none)', function t() {
	var info;
	var tc;
	var A;
	var B;
	var Q;
	var Z;
	var n;

	tc = no_qz_3x3;
	n = 3;
	A = new Float64Array([
		1.0,
		4.0,
		7.0,
		2.0,
		5.0,
		8.0,
		3.0,
		6.0,
		9.0
	]);
	B = new Float64Array([
		2.0,
		0.0,
		0.0,
		1.0,
		3.0,
		0.0,
		0.5,
		1.0,
		1.0
	]);
	Q = new Float64Array( n * n );
	Z = new Float64Array( n * n );
	info = dgghrd( 'none', 'none', n, 1, 3, A, 1, n, 0, B, 1, n, 0, Q, 1, n, 0, Z, 1, n, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0, 'info = 0' );
	assertArrayClose( extractMatrix( A, n ), tc.A, 1e-14, 'A' );
	assertArrayClose( extractMatrix( B, n ), tc.B, 1e-14, 'B' );
});

test( 'dgghrd: partial_5x5 (ILO=2, IHI=4)', function t() {
	var info;
	var tc;
	var A;
	var B;
	var Q;
	var Z;
	var n;
	var i;
	var j;

	tc = partial_5x5;
	n = 5;
	A = new Float64Array( n * n );
	for ( j = 0; j < n; j += 1 ) {
		for ( i = 0; i < n; i += 1 ) {
			A[ i + (j * n) ] = (i + 1) + (j + 1) + (0.5 * ((i + 1) - (j + 1)));
		}
	}
	B = new Float64Array( n * n );
	for ( j = 0; j < n; j += 1 ) {
		for ( i = 0; i <= j; i += 1 ) {
			B[ i + (j * n) ] = (i + 1) + (j + 1) + (0.25 * ((j + 1) - (i + 1)));
		}
	}
	Q = new Float64Array( n * n );
	Z = new Float64Array( n * n );
	info = dgghrd( 'initialize', 'initialize', n, 2, 4, A, 1, n, 0, B, 1, n, 0, Q, 1, n, 0, Z, 1, n, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0, 'info = 0' );
	assertArrayClose( extractMatrix( A, n ), tc.A, 1e-14, 'A' );
	assertArrayClose( extractMatrix( B, n ), tc.B, 1e-14, 'B' );
	assertArrayClose( extractMatrix( Q, n ), tc.Q, 1e-14, 'Q' );
	assertArrayClose( extractMatrix( Z, n ), tc.Z, 1e-14, 'Z' );
});

test( 'dgghrd: accumulate_3x3 (COMPQ=update, COMPZ=update)', function t() {
	var info;
	var tc;
	var A;
	var B;
	var Q;
	var Z;
	var n;
	var i;

	tc = accumulate_3x3;
	n = 3;
	A = new Float64Array([
		2.0,
		3.0,
		1.0,
		1.0,
		1.0,
		0.5,
		0.5,
		2.0,
		3.0
	]);
	B = new Float64Array([
		1.0,
		0.0,
		0.0,
		0.5,
		2.0,
		0.0,
		0.25,
		1.0,
		3.0
	]);
	Q = new Float64Array( n * n );
	Z = new Float64Array( n * n );
	for ( i = 0; i < n; i += 1 ) {
		Q[ i + (i * n) ] = 1.0;
		Z[ i + (i * n) ] = 1.0;
	}
	info = dgghrd( 'update', 'update', n, 1, 3, A, 1, n, 0, B, 1, n, 0, Q, 1, n, 0, Z, 1, n, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0, 'info = 0' );
	assertArrayClose( extractMatrix( A, n ), tc.A, 1e-14, 'A' );
	assertArrayClose( extractMatrix( B, n ), tc.B, 1e-14, 'B' );
	assertArrayClose( extractMatrix( Q, n ), tc.Q, 1e-14, 'Q' );
	assertArrayClose( extractMatrix( Z, n ), tc.Z, 1e-14, 'Z' );
});

test( 'dgghrd: ilo_eq_ihi (ILO=IHI, no reduction needed)', function t() {
	var info;
	var tc;
	var A;
	var B;
	var Q;
	var Z;
	var n;

	tc = ilo_eq_ihi;
	n = 3;
	A = new Float64Array([
		1.0,
		4.0,
		7.0,
		2.0,
		5.0,
		8.0,
		3.0,
		6.0,
		9.0
	]);
	B = new Float64Array([
		1.0,
		0.0,
		0.0,
		1.0,
		2.0,
		0.0,
		1.0,
		1.0,
		3.0
	]);
	Q = new Float64Array( n * n );
	Z = new Float64Array( n * n );
	info = dgghrd( 'initialize', 'initialize', n, 2, 2, A, 1, n, 0, B, 1, n, 0, Q, 1, n, 0, Z, 1, n, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0, 'info = 0' );
	assertArrayClose( extractMatrix( A, n ), tc.A, 1e-14, 'A' );
	assertArrayClose( extractMatrix( B, n ), tc.B, 1e-14, 'B' );
	assertArrayClose( extractMatrix( Q, n ), tc.Q, 1e-14, 'Q' );
	assertArrayClose( extractMatrix( Z, n ), tc.Z, 1e-14, 'Z' );
});

test( 'dgghrd: empty_range (IHI=ILO-1, trivial quick return)', function t() {
	var info;
	var tc;
	var A;
	var B;
	var Q;
	var Z;
	var n;

	tc = empty_range;
	n = 3;
	A = new Float64Array([
		1.0,
		4.0,
		7.0,
		2.0,
		5.0,
		8.0,
		3.0,
		6.0,
		9.0
	]);
	B = new Float64Array([
		1.0,
		0.0,
		0.0,
		0.0,
		2.0,
		0.0,
		0.0,
		0.0,
		3.0
	]);
	Q = new Float64Array( n * n );
	Z = new Float64Array( n * n );
	info = dgghrd( 'initialize', 'initialize', n, 2, 1, A, 1, n, 0, B, 1, n, 0, Q, 1, n, 0, Z, 1, n, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0, 'info = 0' );
	assertArrayClose( extractMatrix( A, n ), tc.A, 1e-14, 'A' );
	assertArrayClose( extractMatrix( B, n ), tc.B, 1e-14, 'B' );
	assertArrayClose( extractMatrix( Q, n ), tc.Q, 1e-14, 'Q' );
	assertArrayClose( extractMatrix( Z, n ), tc.Z, 1e-14, 'Z' );
});

test( 'dgghrd: returns -1 for invalid COMPQ', function t() {
	var info;
	var A;
	var B;
	var Q;
	var Z;

	A = new Float64Array( 4 );
	B = new Float64Array( 4 );
	Q = new Float64Array( 4 );
	Z = new Float64Array( 4 );
	info = dgghrd( 'invalid', 'none', 2, 1, 2, A, 1, 2, 0, B, 1, 2, 0, Q, 1, 2, 0, Z, 1, 2, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, -1, 'returns -1 for invalid compq' );
});

test( 'dgghrd: returns -2 for invalid COMPZ', function t() {
	var info;
	var A;
	var B;
	var Q;
	var Z;

	A = new Float64Array( 4 );
	B = new Float64Array( 4 );
	Q = new Float64Array( 4 );
	Z = new Float64Array( 4 );
	info = dgghrd( 'none', 'invalid', 2, 1, 2, A, 1, 2, 0, B, 1, 2, 0, Q, 1, 2, 0, Z, 1, 2, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, -2, 'returns -2 for invalid compz' );
});

test( 'dgghrd: returns -3 for negative N', function t() {
	var info;
	var A;
	var B;
	var Q;
	var Z;

	A = new Float64Array( 4 );
	B = new Float64Array( 4 );
	Q = new Float64Array( 4 );
	Z = new Float64Array( 4 );
	info = dgghrd( 'none', 'none', -1, 1, 2, A, 1, 2, 0, B, 1, 2, 0, Q, 1, 2, 0, Z, 1, 2, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, -3, 'returns -3 for negative N' );
});

test( 'dgghrd: N=0 returns 0 immediately', function t() {
	var info;
	var A;
	var B;
	var Q;
	var Z;

	A = new Float64Array( 0 );
	B = new Float64Array( 0 );
	Q = new Float64Array( 0 );
	Z = new Float64Array( 0 );
	info = dgghrd( 'none', 'none', 0, 1, 0, A, 1, 1, 0, B, 1, 1, 0, Q, 1, 1, 0, Z, 1, 1, 0 ); // eslint-disable-line max-len
	assert.strictEqual( info, 0, 'returns 0 for N=0' );
});
