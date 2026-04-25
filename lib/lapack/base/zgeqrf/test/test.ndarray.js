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
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgeqrf = require( './../lib/ndarray.js' );

// FIXTURES //

var basic_4x3 = require( './fixtures/basic_4x3.json' );
var square_3x3 = require( './fixtures/square_3x3.json' );
var one_by_one = require( './fixtures/one_by_one.json' );
var tall_5x2 = require( './fixtures/tall_5x2.json' );
var wide_2x5 = require( './fixtures/wide_2x5.json' );
var large_40x35 = require( './fixtures/large_40x35.json' );

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
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

// TESTS //

test( 'zgeqrf: basic 4x3 matrix', function t() {
	var work;
	var info;
	var tau;
	var tc;
	var a;

	tc = basic_4x3;
	a = new Complex128Array([
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
		1,
		2,
		3,
		4,
		5,
		6
	]);
	tau = new Complex128Array( 3 );
	work = new Complex128Array( 100 );
	info = zgeqrf( 4, 3, a, 1, 4, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgeqrf: square 3x3 matrix', function t() {
	var work;
	var info;
	var tau;
	var tc;
	var a;

	tc = square_3x3;
	a = new Complex128Array([
		1,
		1,
		0,
		1,
		1,
		0,
		2,
		0.5,
		1,
		1,
		0.5,
		0.5,
		0,
		1,
		1,
		0,
		2,
		2
	]);
	tau = new Complex128Array( 3 );
	work = new Complex128Array( 100 );
	info = zgeqrf( 3, 3, a, 1, 3, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgeqrf: M=0 quick return', function t() {
	var work;
	var info;
	var tau;
	var a;

	a = new Complex128Array( 2 );
	tau = new Complex128Array( 2 );
	work = new Complex128Array( 10 );
	info = zgeqrf( 0, 3, a, 1, 1, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, 0, 1e-14, 'info' );
});

test( 'zgeqrf: N=0 quick return', function t() {
	var work;
	var info;
	var tau;
	var a;

	a = new Complex128Array( 10 );
	tau = new Complex128Array( 2 );
	work = new Complex128Array( 10 );
	info = zgeqrf( 4, 0, a, 1, 4, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, 0, 1e-14, 'info' );
});

test( 'zgeqrf: 1x1 matrix', function t() {
	var work;
	var info;
	var tau;
	var tc;
	var a;

	tc = one_by_one;
	a = new Complex128Array( [ 5, 3 ] );
	tau = new Complex128Array( 1 );
	work = new Complex128Array( 10 );
	info = zgeqrf( 1, 1, a, 1, 1, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgeqrf: tall 5x2 matrix', function t() {
	var work;
	var info;
	var tau;
	var tc;
	var a;

	tc = tall_5x2;
	a = new Complex128Array([
		1,
		0.5,
		2,
		1,
		3,
		1.5,
		4,
		2,
		5,
		2.5,
		0.5,
		1,
		1.5,
		2,
		2.5,
		3,
		3.5,
		4,
		4.5,
		5
	]);
	tau = new Complex128Array( 2 );
	work = new Complex128Array( 100 );
	info = zgeqrf( 5, 2, a, 1, 5, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgeqrf: wide 2x5 matrix (M < N)', function t() {
	var work;
	var info;
	var tau;
	var tc;
	var a;

	tc = wide_2x5;
	a = new Complex128Array([
		1,
		1,
		2,
		0,
		3,
		2,
		4,
		1,
		5,
		3,
		6,
		0.5,
		7,
		1,
		8,
		2,
		9,
		0,
		0,
		1
	]);
	tau = new Complex128Array( 2 );
	work = new Complex128Array( 100 );
	info = zgeqrf( 2, 5, a, 1, 2, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgeqrf: works with offset', function t() {
	var work;
	var info;
	var pad;
	var tau;
	var tc;
	var av;
	var a;

	tc = square_3x3;
	pad = 2;
	a = new Complex128Array( pad + 9 );
	av = reinterpret( a, 0 );
	av.set([
		1,
		1,
		0,
		1,
		1,
		0,
		2,
		0.5,
		1,
		1,
		0.5,
		0.5,
		0,
		1,
		1,
		0,
		2,
		2
	], pad * 2 );
	tau = new Complex128Array( pad + 3 );
	work = new Complex128Array( 100 );
	info = zgeqrf( 3, 3, a, 1, 3, pad, tau, 1, pad, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose(reinterpret( a, 0 ).subarray( pad * 2, pad * 2 + 18 ), tc.a, 1e-10, 'a with offset');
	assertArrayClose(reinterpret( tau, 0 ).subarray( pad * 2, pad * 2 + 6 ), tc.tau, 1e-10, 'tau with offset');
});

test( 'zgeqrf: null WORK triggers internal allocation', function t() {
	var info;
	var tau;
	var tc;
	var a;

	tc = basic_4x3;
	a = new Complex128Array([
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
		1,
		2,
		3,
		4,
		5,
		6
	]);
	tau = new Complex128Array( 3 );
	info = zgeqrf( 4, 3, a, 1, 4, 0, tau, 1, 0, null, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgeqrf: large 40x35 matrix (blocked path)', function t() {
	var work;
	var info;
	var idx;
	var tau;
	var tc;
	var av;
	var M;
	var N;
	var a;
	var i;
	var j;

	tc = large_40x35;
	M = 40;
	N = 35;
	a = new Complex128Array( M * N );
	av = reinterpret( a, 0 );
	for ( j = 1; j <= N; j++ ) {
		for ( i = 1; i <= M; i++ ) {
			idx = 2 * ( ( i - 1 ) + ( j - 1 ) * M );
			av[ idx ] = ( ( i * 7 + j * 13 ) % 100 ) / 10.0;
			av[ idx + 1 ] = ( ( i * 11 + j * 3 ) % 100 ) / 10.0;
		}
	}
	tau = new Complex128Array( N );
	work = new Complex128Array( N * 64 );
	info = zgeqrf( M, N, a, 1, M, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});
