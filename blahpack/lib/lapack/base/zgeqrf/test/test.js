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
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zgeqrf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgeqrf.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'zgeqrf: basic 4x3 matrix', function t() {
	var tc = findCase( 'basic_4x3' );
	// 4x3 complex column-major interleaved: A(i,j) at index 2*(i + j*4)
	var a = new Float64Array([
		1, 2, 3, 4, 5, 6, 7, 8,  // col 0: (1+2i), (3+4i), (5+6i), (7+8i)
		9, 1, 2, 3, 4, 5, 6, 7,  // col 1: (9+1i), (2+3i), (4+5i), (6+7i)
		8, 9, 1, 2, 3, 4, 5, 6   // col 2: (8+9i), (1+2i), (3+4i), (5+6i)
	]);
	var tau = new Float64Array( 6 );
	var work = new Float64Array( 200 );
	var info = zgeqrf( 4, 3, a, 1, 4, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( a, tc.a, 1e-10, 'a' );
	assertArrayClose( tau, tc.tau, 1e-10, 'tau' );
});

test( 'zgeqrf: square 3x3 matrix', function t() {
	var tc = findCase( 'square_3x3' );
	// 3x3 complex column-major interleaved
	var a = new Float64Array([
		1, 1, 0, 1, 1, 0,        // col 0: (1+1i), (0+1i), (1+0i)
		2, 0.5, 1, 1, 0.5, 0.5,  // col 1: (2+0.5i), (1+1i), (0.5+0.5i)
		0, 1, 1, 0, 2, 2         // col 2: (0+1i), (1+0i), (2+2i)
	]);
	var tau = new Float64Array( 6 );
	var work = new Float64Array( 200 );
	var info = zgeqrf( 3, 3, a, 1, 3, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( a, tc.a, 1e-10, 'a' );
	assertArrayClose( tau, tc.tau, 1e-10, 'tau' );
});

test( 'zgeqrf: M=0 quick return', function t() {
	var a = new Float64Array( 4 );
	var tau = new Float64Array( 4 );
	var work = new Float64Array( 20 );
	var info = zgeqrf( 0, 3, a, 1, 1, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, 0, 1e-14, 'info' );
});

test( 'zgeqrf: N=0 quick return', function t() {
	var a = new Float64Array( 20 );
	var tau = new Float64Array( 4 );
	var work = new Float64Array( 20 );
	var info = zgeqrf( 4, 0, a, 1, 4, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, 0, 1e-14, 'info' );
});

test( 'zgeqrf: 1x1 matrix', function t() {
	var tc = findCase( 'one_by_one' );
	var a = new Float64Array( [ 5, 3 ] );
	var tau = new Float64Array( 2 );
	var work = new Float64Array( 20 );
	var info = zgeqrf( 1, 1, a, 1, 1, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( a, tc.a, 1e-10, 'a' );
	assertArrayClose( tau, tc.tau, 1e-10, 'tau' );
});

test( 'zgeqrf: tall 5x2 matrix', function t() {
	var tc = findCase( 'tall_5x2' );
	// 5x2 complex column-major interleaved
	var a = new Float64Array([
		1, 0.5, 2, 1, 3, 1.5, 4, 2, 5, 2.5,     // col 0
		0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5   // col 1
	]);
	var tau = new Float64Array( 4 );
	var work = new Float64Array( 200 );
	var info = zgeqrf( 5, 2, a, 1, 5, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( a, tc.a, 1e-10, 'a' );
	assertArrayClose( tau, tc.tau, 1e-10, 'tau' );
});

test( 'zgeqrf: wide 2x5 matrix (M < N)', function t() {
	var tc = findCase( 'wide_2x5' );
	// 2x5 complex column-major interleaved
	var a = new Float64Array([
		1, 1, 2, 0,        // col 0: (1+1i), (2+0i)
		3, 2, 4, 1,        // col 1: (3+2i), (4+1i)
		5, 3, 6, 0.5,      // col 2: (5+3i), (6+0.5i)
		7, 1, 8, 2,        // col 3: (7+1i), (8+2i)
		9, 0, 0, 1         // col 4: (9+0i), (0+1i)
	]);
	var tau = new Float64Array( 4 );
	var work = new Float64Array( 200 );
	var info = zgeqrf( 2, 5, a, 1, 2, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( a, tc.a, 1e-10, 'a' );
	assertArrayClose( tau, tc.tau, 1e-10, 'tau' );
});

test( 'zgeqrf: works with offset', function t() {
	var tc = findCase( 'square_3x3' );
	// Pad with 4 doubles (2 complex elements) at the start
	var pad = 4;
	var a = new Float64Array( pad + 18 );
	a.set([
		1, 1, 0, 1, 1, 0,
		2, 0.5, 1, 1, 0.5, 0.5,
		0, 1, 1, 0, 2, 2
	], pad );
	var tau = new Float64Array( pad + 6 );
	var work = new Float64Array( 200 );
	var info = zgeqrf( 3, 3, a, 1, 3, pad, tau, 1, pad, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose(
		a.subarray( pad, pad + 18 ),
		tc.a,
		1e-10,
		'a with offset'
	);
	assertArrayClose(
		tau.subarray( pad, pad + 6 ),
		tc.tau,
		1e-10,
		'tau with offset'
	);
});

test( 'zgeqrf: null WORK triggers internal allocation', function t() {
	var tc = findCase( 'basic_4x3' );
	var a = new Float64Array([
		1, 2, 3, 4, 5, 6, 7, 8,
		9, 1, 2, 3, 4, 5, 6, 7,
		8, 9, 1, 2, 3, 4, 5, 6
	]);
	var tau = new Float64Array( 6 );
	var info = zgeqrf( 4, 3, a, 1, 4, 0, tau, 1, 0, null, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( a, tc.a, 1e-10, 'a' );
	assertArrayClose( tau, tc.tau, 1e-10, 'tau' );
});

test( 'zgeqrf: large 40x35 matrix (blocked path)', function t() {
	var tc = findCase( 'large_40x35' );
	var M = 40;
	var N = 35;
	var a = new Float64Array( 2 * M * N );
	var i;
	var j;
	var idx;
	// A(i,j) = ((i*7 + j*13) mod 100) / 10 + ((i*11 + j*3) mod 100) / 10 * 1i
	// Fortran 1-based: i from 1..M, j from 1..N
	for ( j = 1; j <= N; j++ ) {
		for ( i = 1; i <= M; i++ ) {
			idx = 2 * ( ( i - 1 ) + ( j - 1 ) * M );
			a[ idx ] = ( ( i * 7 + j * 13 ) % 100 ) / 10.0;
			a[ idx + 1 ] = ( ( i * 11 + j * 3 ) % 100 ) / 10.0;
		}
	}
	var tau = new Float64Array( 2 * N );
	var work = new Float64Array( 2 * N * 64 );
	var info = zgeqrf( M, N, a, 1, M, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( a, tc.a, 1e-10, 'a' );
	assertArrayClose( tau, tc.tau, 1e-10, 'tau' );
});
