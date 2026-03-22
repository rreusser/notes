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
var zgeqr2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgeqr2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zgeqr2: basic 3x2 matrix', function t() {
	var tc = findCase( 'basic_3x2' );
	// A = [1+0i 4+1i; 2+0i 5+1i; 3+0i 6+1i] col-major interleaved
	var a = new Complex128Array( [ 1,0, 2,0, 3,0, 4,1, 5,1, 6,1 ] );
	var tau = new Complex128Array( 2 );
	var work = new Complex128Array( 10 );
	var info = zgeqr2( 3, 2, a, 1, 3, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgeqr2: square 2x2 matrix', function t() {
	var tc = findCase( 'square_2x2' );
	// A = [1+1i 1+0i; 0+1i 1+1i] col-major interleaved
	var a = new Complex128Array( [ 1,1, 0,1, 1,0, 1,1 ] );
	var tau = new Complex128Array( 2 );
	var work = new Complex128Array( 10 );
	var info = zgeqr2( 2, 2, a, 1, 2, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgeqr2: M=0 quick return', function t() {
	var a = new Complex128Array( 2 );
	var tau = new Complex128Array( 2 );
	var work = new Complex128Array( 2 );
	var info = zgeqr2( 0, 2, a, 1, 1, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, 0, 1e-14, 'info' );
});

test( 'zgeqr2: N=0 quick return', function t() {
	var a = new Complex128Array( 6 );
	var tau = new Complex128Array( 2 );
	var work = new Complex128Array( 2 );
	var info = zgeqr2( 3, 0, a, 1, 3, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, 0, 1e-14, 'info' );
});

test( 'zgeqr2: 1x1 matrix', function t() {
	var tc = findCase( 'one_by_one' );
	var a = new Complex128Array( [ 5, 3 ] );
	var tau = new Complex128Array( 1 );
	var work = new Complex128Array( 2 );
	var info = zgeqr2( 1, 1, a, 1, 1, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});
