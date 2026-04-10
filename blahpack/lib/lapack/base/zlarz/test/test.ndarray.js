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

/* eslint-disable no-restricted-syntax, stdlib/first-unit-test, max-len */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlarz.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'ndarray export is a function', function t() {
	assert.strictEqual( typeof ndarray, 'function', 'is a function' );
});

test( 'ndarray: throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		ndarray( 'invalid', 2, 2, 1, new Complex128Array( 2 ), 1, 0, new Complex128Array( 1 ), 0, new Complex128Array( 4 ), 1, 2, 0, new Complex128Array( 2 ), 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		ndarray( 'left', -1, 2, 1, new Complex128Array( 2 ), 1, 0, new Complex128Array( 1 ), 0, new Complex128Array( 4 ), 1, 2, 0, new Complex128Array( 2 ), 1, 0 );
	}, RangeError );
});

test( 'ndarray: throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		ndarray( 'left', 2, -1, 1, new Complex128Array( 2 ), 1, 0, new Complex128Array( 1 ), 0, new Complex128Array( 4 ), 1, 2, 0, new Complex128Array( 2 ), 1, 0 );
	}, RangeError );
});

test( 'ndarray: returns C unchanged when M=0', function t() {
	var C = new Complex128Array( [ 1.0, 2.0 ] );
	var out = ndarray( 'left', 0, 1, 0, new Complex128Array( 1 ), 1, 0, new Complex128Array( 1 ), 0, C, 1, 1, 0, new Complex128Array( 1 ), 1, 0 );
	assert.strictEqual( out, C, 'returns C' );
});

test( 'ndarray: returns C unchanged when N=0', function t() {
	var C = new Complex128Array( [ 1.0, 2.0 ] );
	var out = ndarray( 'left', 1, 0, 0, new Complex128Array( 1 ), 1, 0, new Complex128Array( 1 ), 0, C, 1, 1, 0, new Complex128Array( 1 ), 1, 0 );
	assert.strictEqual( out, C, 'returns C' );
});

test( 'ndarray: left side, column-major strides (baseline), matches fixture', function t() {
	var tc = findCase( 'zlarz_left_4x3_l2' );
	var v = new Complex128Array( [ 0.5, 0.2, -0.3, 0.4 ] );
	var tau = new Complex128Array( [ 1.2, -0.4 ] );
	var C = new Complex128Array( [
		1.0, 0.0, 2.0, 1.0, 3.0, -1.0, 4.0, 0.5,
		-1.0, 2.0, 0.5, 0.5, 1.5, -0.5, -2.0, 1.0,
		0.0, 1.0, 1.0, 1.0, -0.5, 0.0, 2.0, -2.0
	] );
	var WORK = new Complex128Array( 3 );
	ndarray( 'left', 4, 3, 2, v, 1, 0, tau, 0, C, 1, 4, 0, WORK, 1, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-13, 'C' );
});

test( 'ndarray: left side, row-major strides (strideC1=N, strideC2=1), matches fixture', function t() {
	// Same logical 4x3 matrix as zlarz_left_4x3_l2, stored row-major.
	var tc = findCase( 'zlarz_left_4x3_l2' );
	var v = new Complex128Array( [ 0.5, 0.2, -0.3, 0.4 ] );
	var tau = new Complex128Array( [ 1.2, -0.4 ] );

	// logical (i,j) at row-major index 3*i + j
	// Build from column-major fixture input values:
	var colIn = [
		1.0, 0.0, 2.0, 1.0, 3.0, -1.0, 4.0, 0.5,
		-1.0, 2.0, 0.5, 0.5, 1.5, -0.5, -2.0, 1.0,
		0.0, 1.0, 1.0, 1.0, -0.5, 0.0, 2.0, -2.0
	];
	var rowIn = new Array( 24 );
	var i;
	var j;
	for ( i = 0; i < 4; i++ ) {
		for ( j = 0; j < 3; j++ ) {
			rowIn[ 2 * ( ( 3 * i ) + j ) ] = colIn[ 2 * ( i + ( 4 * j ) ) ];
			rowIn[ ( 2 * ( ( 3 * i ) + j ) ) + 1 ] = colIn[ ( 2 * ( i + ( 4 * j ) ) ) + 1 ];
		}
	}
	var C = new Complex128Array( rowIn );
	var WORK = new Complex128Array( 3 );
	ndarray( 'left', 4, 3, 2, v, 1, 0, tau, 0, C, 3, 1, 0, WORK, 1, 0 );

	// Permute fixture output from col-major to row-major
	var expected = new Array( 24 );
	for ( i = 0; i < 4; i++ ) {
		for ( j = 0; j < 3; j++ ) {
			expected[ 2 * ( ( 3 * i ) + j ) ] = tc.C[ 2 * ( i + ( 4 * j ) ) ];
			expected[ ( 2 * ( ( 3 * i ) + j ) ) + 1 ] = tc.C[ ( 2 * ( i + ( 4 * j ) ) ) + 1 ];
		}
	}
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), expected, 1e-13, 'C' );
});

test( 'ndarray: left side with offsets + padded v/tau/WORK, sentinels untouched', function t() {
	var tc = findCase( 'zlarz_left_4x3_l2' );

	// v padded front (1 complex) + back (2 complex sentinels)
	var v = new Complex128Array( [
		99.0, -99.0,     // pad front
		0.5, 0.2, -0.3, 0.4,
		77.0, -77.0, 88.0, -88.0 // pad back
	] );
	// tau padded front (1 complex) + back (1 complex sentinel)
	var tau = new Complex128Array( [
		11.0, -11.0,
		1.2, -0.4,
		22.0, -22.0
	] );
	// C padded front (1 complex) + back (2 complex)
	var Cin = [
		1.0, 0.0, 2.0, 1.0, 3.0, -1.0, 4.0, 0.5,
		-1.0, 2.0, 0.5, 0.5, 1.5, -0.5, -2.0, 1.0,
		0.0, 1.0, 1.0, 1.0, -0.5, 0.0, 2.0, -2.0
	];
	var head = [ 13.0, -13.0 ]; // 1 complex pad front
	var tail = [ 17.0, -17.0, 19.0, -19.0 ]; // 2 complex pad back
	var Cbuf = head.concat( Cin ).concat( tail );
	var C = new Complex128Array( Cbuf );

	// WORK padded front + back
	var WORK = new Complex128Array( [
		33.0, -33.0,
		0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		44.0, -44.0
	] );

	ndarray( 'left', 4, 3, 2, v, 1, 1, tau, 1, C, 1, 4, 1, WORK, 1, 1 );

	// Check C body matches fixture
	var full = Array.from( reinterpret( C, 0 ) );
	var body = full.slice( 2, 2 + 24 );
	assertArrayClose( body, tc.C, 1e-13, 'C body' );

	// Sentinels untouched
	assert.strictEqual( full[ 0 ], 13.0, 'C head re untouched' );
	assert.strictEqual( full[ 1 ], -13.0, 'C head im untouched' );
	assert.strictEqual( full[ 26 ], 17.0, 'C tail[0] re untouched' );
	assert.strictEqual( full[ 27 ], -17.0, 'C tail[0] im untouched' );
	assert.strictEqual( full[ 28 ], 19.0, 'C tail[1] re untouched' );
	assert.strictEqual( full[ 29 ], -19.0, 'C tail[1] im untouched' );

	// v sentinels untouched. v layout (real): [99,-99, 0.5,0.2,-0.3,0.4, 77,-77,88,-88]
	var vfull = Array.from( reinterpret( v, 0 ) );
	assert.strictEqual( vfull[ 0 ], 99.0, 'v head re untouched' );
	assert.strictEqual( vfull[ 1 ], -99.0, 'v head im untouched' );
	assert.strictEqual( vfull[ 6 ], 77.0, 'v tail[0] re untouched' );
	assert.strictEqual( vfull[ 7 ], -77.0, 'v tail[0] im untouched' );
	assert.strictEqual( vfull[ 8 ], 88.0, 'v tail[1] re untouched' );
	assert.strictEqual( vfull[ 9 ], -88.0, 'v tail[1] im untouched' );

	// tau sentinels untouched
	var tfull = Array.from( reinterpret( tau, 0 ) );
	assert.strictEqual( tfull[ 0 ], 11.0, 'tau head re untouched' );
	assert.strictEqual( tfull[ 5 ], -22.0, 'tau tail im untouched' );
});

test( 'ndarray: right side with non-unit strideV and offsetV', function t() {
	var tc = findCase( 'zlarz_right_3x4_l2' );

	// Pack v with stride 2: real v at even complex indices
	// Logical v = [0.5+0.2i, -0.3+0.4i]; store at v[0], v[2] with junk at v[1]
	var v = new Complex128Array( [
		0.5, 0.2,
		55.0, -55.0, // filler
		-0.3, 0.4
	] );
	var tau = new Complex128Array( [ 0.8, 0.5 ] );
	var C = new Complex128Array( [
		1.0, 0.0, 2.0, 1.0, 3.0, -1.0,
		4.0, 0.5, -1.0, 2.0, 0.5, 0.5,
		1.5, -0.5, -2.0, 1.0, 0.0, 1.0,
		1.0, 1.0, -0.5, 0.0, 2.0, -2.0
	] );
	var WORK = new Complex128Array( 3 );

	ndarray( 'right', 3, 4, 2, v, 2, 0, tau, 0, C, 1, 3, 0, WORK, 1, 0 );

	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-13, 'C' );

	// Verify filler was not overwritten
	var vfull = Array.from( reinterpret( v, 0 ) );
	assert.strictEqual( vfull[ 2 ], 55.0, 'v filler re untouched' );
	assert.strictEqual( vfull[ 3 ], -55.0, 'v filler im untouched' );
});
