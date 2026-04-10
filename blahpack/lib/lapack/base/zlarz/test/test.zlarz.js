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
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlarz = require( './../lib/zlarz.js' );


// FUNCTIONS //

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	var relErr;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
	}
}


// TESTS //

test( 'zlarz is a function', function t() {
	assert.strictEqual( typeof zlarz, 'function', 'is a function' );
});

test( 'zlarz has expected arity', function t() {
	assert.strictEqual( zlarz.length, 13, 'has expected arity' );
});

test( 'zlarz throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zlarz( 'invalid', 'left', 2, 2, 1, new Complex128Array( 2 ), 1, new Complex128Array( 1 ), 0, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1 );
	}, TypeError );
});

test( 'zlarz throws TypeError for invalid side', function t() {
	assert.throws( function throws() {
		zlarz( 'column-major', 'invalid', 2, 2, 1, new Complex128Array( 2 ), 1, new Complex128Array( 1 ), 0, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1 );
	}, TypeError );
});

test( 'zlarz throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlarz( 'column-major', 'left', -1, 2, 1, new Complex128Array( 2 ), 1, new Complex128Array( 1 ), 0, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1 );
	}, RangeError );
});

test( 'zlarz throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlarz( 'column-major', 'left', 2, -1, 1, new Complex128Array( 2 ), 1, new Complex128Array( 1 ), 0, new Complex128Array( 4 ), 2, new Complex128Array( 2 ), 1 );
	}, RangeError );
});

test( 'zlarz throws RangeError for LDC too small (column-major)', function t() {
	assert.throws( function throws() {
		zlarz( 'column-major', 'left', 4, 3, 2, new Complex128Array( 2 ), 1, new Complex128Array( 1 ), 0, new Complex128Array( 12 ), 2, new Complex128Array( 3 ), 1 );
	}, RangeError );
});

test( 'zlarz throws RangeError for LDC too small (row-major)', function t() {
	assert.throws( function throws() {
		zlarz( 'row-major', 'left', 4, 3, 2, new Complex128Array( 2 ), 1, new Complex128Array( 1 ), 0, new Complex128Array( 12 ), 2, new Complex128Array( 3 ), 1 );
	}, RangeError );
});

test( 'zlarz column-major, left side, matches fixture', function t() {
	var v = new Complex128Array( [ 0.5, 0.2, -0.3, 0.4 ] );
	var tau = new Complex128Array( [ 1.2, -0.4 ] );
	var C = new Complex128Array( [
		1.0, 0.0, 2.0, 1.0, 3.0, -1.0, 4.0, 0.5,
		-1.0, 2.0, 0.5, 0.5, 1.5, -0.5, -2.0, 1.0,
		0.0, 1.0, 1.0, 1.0, -0.5, 0.0, 2.0, -2.0
	] );
	var WORK = new Complex128Array( 3 );
	var out = zlarz( 'column-major', 'left', 4, 3, 2, v, 1, tau, 0, C, 4, WORK, 1 );
	assert.strictEqual( out, C, 'returns C' );

	var expected = [
		0.58, 3.94, 2.0, 1.0, 2.002, 0.886, 2.55, -0.85,
		-2.56, -0.08, 0.5, 0.5, 1.136, -1.852, -0.7, 1.0,
		1.62, -0.74, 1.0, 1.0, 0.658, -0.546, 2.21, -0.83
	];
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), expected, 1e-12, 'C' );
});

test( 'zlarz row-major, left side, matches column-major permutation', function t() {
	// Run the 4x3 left-side test in row-major layout: same logical matrix,
	// transposed storage. Fixture values are from zlarz_left_4x3_l2.
	var v = new Complex128Array( [ 0.5, 0.2, -0.3, 0.4 ] );
	var tau = new Complex128Array( [ 1.2, -0.4 ] );

	// Logical 4x3 matrix:
	// row 0: 1+0i, -1+2i, 0+1i
	// row 1: 2+1i, 0.5+0.5i, 1+1i
	// row 2: 3-1i, 1.5-0.5i, -0.5+0i
	// row 3: 4+0.5i, -2+1i, 2-2i
	// Row-major: row 0, row 1, row 2, row 3
	var C = new Complex128Array( [
		1.0, 0.0, -1.0, 2.0, 0.0, 1.0,
		2.0, 1.0, 0.5, 0.5, 1.0, 1.0,
		3.0, -1.0, 1.5, -0.5, -0.5, 0.0,
		4.0, 0.5, -2.0, 1.0, 2.0, -2.0
	] );
	var WORK = new Complex128Array( 3 );
	zlarz( 'row-major', 'left', 4, 3, 2, v, 1, tau, 0, C, 3, WORK, 1 );

	// Expected (row-major) reconstructed from the column-major fixture.
	// Column-major flat expected: for logical (i,j), index = i + 4*j
	// Row-major flat: index = 3*i + j
	var colMajor = [
		0.58, 3.94, 2.0, 1.0, 2.002, 0.886, 2.55, -0.85,
		-2.56, -0.08, 0.5, 0.5, 1.136, -1.852, -0.7, 1.0,
		1.62, -0.74, 1.0, 1.0, 0.658, -0.546, 2.21, -0.83
	];
	var expected = new Array( 24 );
	var i;
	var j;
	for ( i = 0; i < 4; i++ ) {
		for ( j = 0; j < 3; j++ ) {
			expected[ 2 * ( ( 3 * i ) + j ) ]     = colMajor[ 2 * ( i + ( 4 * j ) ) ];
			expected[ ( 2 * ( ( 3 * i ) + j ) ) + 1 ] = colMajor[ ( 2 * ( i + ( 4 * j ) ) ) + 1 ];
		}
	}
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), expected, 1e-12, 'C' );
});

test( 'zlarz returns C unchanged when M=0', function t() {
	var C = new Complex128Array( [ 1.0, 2.0 ] );
	var out = zlarz( 'column-major', 'left', 0, 1, 0, new Complex128Array( 1 ), 1, new Complex128Array( 1 ), 0, C, 1, new Complex128Array( 1 ), 1 );
	assert.strictEqual( out, C, 'returns C' );
});
