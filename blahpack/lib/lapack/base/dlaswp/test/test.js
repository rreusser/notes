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
var dlaswp = require( './../lib' );

test( 'dlaswp: main export is a function', function t() {
	assert.strictEqual( typeof dlaswp, 'function' );
});

test( 'dlaswp: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlaswp.ndarray, 'function' );
});

// FIXTURES //

var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlaswp.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

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
	var tc = findCase( 'basic_forward' );
	// A = [1 4; 2 5; 3 6] col-major (3x2)
	// Fortran: ipiv=[3,2] k1=1 k2=2 → 0-based: ipiv=[2,1] k1=0 k2=1
	var a = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var ipiv = new Int32Array( [ 2, 1 ] );
	dlaswp.ndarray( 2, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	assertArrayClose( a, tc.a, 'basic_forward' );
});

test( 'dlaswp.ndarray is a no-op when ipiv(k) == k', function t() {
	var tc = findCase( 'no_swap' );
	var a = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var ipiv = new Int32Array( [ 0, 1 ] );
	dlaswp.ndarray( 2, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	assertArrayClose( a, tc.a, 'no_swap' );
});

// TODO: Reverse pivots (incx=-1) test — the 0-based/1-based boundary for
// reverse IPIV application is subtle. The ndarray.js wrapper handles normalization.
// Skipping until ndarray.js is implemented.

test( 'dlaswp.ndarray is a no-op when incx=0', function t() {
	var tc = findCase( 'incx_zero' );
	var a = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var ipiv = new Int32Array( [ 2 ] );
	dlaswp.ndarray( 2, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 0 );
	assertArrayClose( a, tc.a, 'incx_zero' );
});

test( 'dlaswp.ndarray applies sequential swaps', function t() {
	var tc = findCase( 'two_swaps' );
	var a = new Float64Array( [ 10, 20, 30 ] );
	var ipiv = new Int32Array( [ 1, 2 ] );
	dlaswp.ndarray( 1, a, 1, 3, 0, 0, 1, ipiv, 1, 0, 1 );
	assertArrayClose( a, tc.a, 'two_swaps' );
});
