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
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgelqf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgelqf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zgelqf: basic 3x5 matrix', function t() {
	var tc = findCase( 'basic_3x5' );
	var a = new Complex128Array([
		1, 2, 3, 4, 5, 6,
		7, 8, 9, 1, 2, 3,
		4, 5, 6, 7, 8, 9,
		1, 2, 3, 4, 5, 6,
		7, 1, 2, 3, 4, 5
	]);
	var tau = new Complex128Array( 3 );
	var work = new Complex128Array( 100 );
	var info = zgelqf( 3, 5, a, 1, 3, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgelqf: square 4x4 matrix', function t() {
	var tc = findCase( 'square_4x4' );
	var a = new Complex128Array([
		1, 1, 0, 1, 1, 0, 2, 0.5,
		2, 0.5, 1, 1, 0.5, 0.5, 3, 1,
		0, 1, 1, 0, 2, 2, 1, 1,
		3, 0, 0.5, 1.5, 1, 2, 4, 0.5
	]);
	var tau = new Complex128Array( 4 );
	var work = new Complex128Array( 100 );
	var info = zgelqf( 4, 4, a, 1, 4, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgelqf: M=0 quick return', function t() {
	var a = new Complex128Array( 2 );
	var tau = new Complex128Array( 2 );
	var work = new Complex128Array( 10 );
	var info = zgelqf( 0, 5, a, 1, 1, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, 0, 1e-14, 'info' );
});

test( 'zgelqf: N=0 quick return', function t() {
	var a = new Complex128Array( 10 );
	var tau = new Complex128Array( 2 );
	var work = new Complex128Array( 10 );
	var info = zgelqf( 3, 0, a, 1, 3, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, 0, 1e-14, 'info' );
});

test( 'zgelqf: 1x1 matrix', function t() {
	var tc = findCase( 'one_by_one' );
	var a = new Complex128Array( [ 5, 3 ] );
	var tau = new Complex128Array( 1 );
	var work = new Complex128Array( 10 );
	var info = zgelqf( 1, 1, a, 1, 1, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgelqf: wide 2x5 matrix', function t() {
	var tc = findCase( 'wide_2x5' );
	var a = new Complex128Array([
		1, 1, 2, 0,
		3, 2, 4, 1,
		5, 3, 6, 0.5,
		7, 1, 8, 2,
		9, 0, 0, 1
	]);
	var tau = new Complex128Array( 2 );
	var work = new Complex128Array( 100 );
	var info = zgelqf( 2, 5, a, 1, 2, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgelqf: tall 4x3 matrix (M > N)', function t() {
	var tc = findCase( 'tall_4x3' );
	var a = new Complex128Array([
		1, 2, 3, 4, 5, 6, 7, 8,
		9, 1, 2, 3, 4, 5, 6, 7,
		8, 9, 1, 2, 3, 4, 5, 6
	]);
	var tau = new Complex128Array( 3 );
	var work = new Complex128Array( 100 );
	var info = zgelqf( 4, 3, a, 1, 4, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgelqf: works with offset', function t() {
	var tc = findCase( 'square_4x4' );
	var pad = 2;
	var a = new Complex128Array( pad + 16 );
	var av = reinterpret( a, 0 );
	av.set([
		1, 1, 0, 1, 1, 0, 2, 0.5,
		2, 0.5, 1, 1, 0.5, 0.5, 3, 1,
		0, 1, 1, 0, 2, 2, 1, 1,
		3, 0, 0.5, 1.5, 1, 2, 4, 0.5
	], pad * 2 );
	var tau = new Complex128Array( pad + 4 );
	var work = new Complex128Array( 100 );
	var info = zgelqf( 4, 4, a, 1, 4, pad, tau, 1, pad, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose(
		reinterpret( a, 0 ).subarray( pad * 2, pad * 2 + 32 ),
		tc.a,
		1e-10,
		'a with offset'
	);
	assertArrayClose(
		reinterpret( tau, 0 ).subarray( pad * 2, pad * 2 + 8 ),
		tc.tau,
		1e-10,
		'tau with offset'
	);
});

test( 'zgelqf: null WORK triggers internal allocation', function t() {
	var tc = findCase( 'basic_3x5' );
	var a = new Complex128Array([
		1, 2, 3, 4, 5, 6,
		7, 8, 9, 1, 2, 3,
		4, 5, 6, 7, 8, 9,
		1, 2, 3, 4, 5, 6,
		7, 1, 2, 3, 4, 5
	]);
	var tau = new Complex128Array( 3 );
	var info = zgelqf( 3, 5, a, 1, 3, 0, tau, 1, 0, null, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgelqf: large 35x50 matrix (blocked path)', function t() {
	var tc = findCase( 'large_35x50' );
	var M = 35;
	var N = 50;
	var a = new Complex128Array( M * N );
	var av = reinterpret( a, 0 );
	var i;
	var j;
	var idx;
	for ( j = 1; j <= N; j++ ) {
		for ( i = 1; i <= M; i++ ) {
			idx = 2 * ( ( i - 1 ) + ( j - 1 ) * M );
			av[ idx ] = ( ( i * 7 + j * 13 ) % 100 ) / 10.0;
			av[ idx + 1 ] = ( ( i * 11 + j * 3 ) % 100 ) / 10.0;
		}
	}
	var tau = new Complex128Array( M );
	var work = new Complex128Array( M * 64 );
	var info = zgelqf( M, N, a, 1, M, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});
