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
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zlascl = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlascl.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// HELPERS //

function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= 1e-14, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, msg ) {
	var i;
	assert.strictEqual( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], msg + '[' + i + ']' );
	}
}

function extractCMatrix( A, strideA1, strideA2, offsetA, M, N ) {
	var out = [];
	var ai;
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			ai = offsetA + 2 * ( i * strideA1 + j * strideA2 );
			out.push( A[ ai ] );
			out.push( A[ ai + 1 ] );
		}
	}
	return out;
}

// TESTS //

test( 'zlascl: main export is a function', function t() {
	assert.strictEqual( typeof zlascl, 'function' );
});

test( 'zlascl: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlascl.ndarray, 'function' );
});

test( 'zlascl: basic scaling (multiply by 2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_basic'; } );
	// A col-major 2x2: [(1+2i), (3+4i), (5+6i), (7+8i)]
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
	var info = base( 'G', 0, 0, 1.0, 2.0, 2, 2, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 'zlascl_basic a' );
});

test( 'zlascl: scaling by 0.5 (cfrom=2, cto=1)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_half'; } );
	var A = new Float64Array( [ 2, 4, 6, 8, 10, 12, 14, 16 ] );
	var info = base( 'G', 0, 0, 2.0, 1.0, 2, 2, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 'zlascl_half a' );
});

test( 'zlascl: M=0 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_m_zero'; } );
	var A = new Float64Array( [ 99, 88 ] );
	var info = base( 'G', 0, 0, 1.0, 2.0, 0, 2, A, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 'zlascl_m_zero a' );
});

test( 'zlascl: upper triangular', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_upper'; } );
	// A col-major 3x3 upper triangular:
	// (1,0) (2,0) (3,0)
	// (0,0) (4,0) (5,0)
	// (0,0) (0,0) (6,0)
	var A = new Float64Array( [
		1, 0,  0, 0,  0, 0,   // col 0
		2, 0,  4, 0,  0, 0,   // col 1
		3, 0,  5, 0,  6, 0    // col 2
	] );
	var info = base( 'U', 0, 0, 1.0, 3.0, 3, 3, A, 1, 3, 0 );
	assert.strictEqual( info, tc.info );
	var result = extractCMatrix( A, 1, 3, 0, 3, 3 );
	assertArrayClose( result, tc.a, 'zlascl_upper a' );
});

test( 'zlascl: lower triangular', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_lower'; } );
	// A col-major 3x3 lower triangular:
	// (1,0) (0,0) (0,0)
	// (2,0) (4,0) (0,0)
	// (3,0) (5,0) (6,0)
	var A = new Float64Array( [
		1, 0,  2, 0,  3, 0,   // col 0
		0, 0,  4, 0,  5, 0,   // col 1
		0, 0,  0, 0,  6, 0    // col 2
	] );
	var info = base( 'L', 0, 0, 1.0, 3.0, 3, 3, A, 1, 3, 0 );
	assert.strictEqual( info, tc.info );
	var result = extractCMatrix( A, 1, 3, 0, 3, 3 );
	assertArrayClose( result, tc.a, 'zlascl_lower a' );
});

test( 'zlascl: identity (cfrom=cto)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_identity'; } );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
	var info = base( 'G', 0, 0, 5.0, 5.0, 2, 2, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 'zlascl_identity a' );
});

test( 'zlascl: upper Hessenberg', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_hessenberg'; } );
	// A col-major 3x3 upper Hessenberg:
	// (1,0) (3,0) (6,0)
	// (2,0) (4,0) (7,0)
	// (0,0) (5,0) (8,0)
	var A = new Float64Array( [
		1, 0,  2, 0,  0, 0,   // col 0
		3, 0,  4, 0,  5, 0,   // col 1
		6, 0,  7, 0,  8, 0    // col 2
	] );
	var info = base( 'H', 0, 0, 1.0, 2.0, 3, 3, A, 1, 3, 0 );
	assert.strictEqual( info, tc.info );
	var result = extractCMatrix( A, 1, 3, 0, 3, 3 );
	assertArrayClose( result, tc.a, 'zlascl_hessenberg a' );
});

test( 'zlascl: large cfrom/cto ratio (iterative scaling, mul=smlnum)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_large_ratio'; } );
	// cfrom=1e300, cto=1e-300 => underflows to 0
	var A = new Float64Array( [ 1, 1, 2, 3 ] );
	var info = base( 'G', 0, 0, 1e300, 1e-300, 2, 1, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 'zlascl_large_ratio a' );
});

test( 'zlascl: large cto/cfrom ratio (iterative scaling, mul=bignum)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_large_ratio_inv'; } );
	// cfrom=1e-150, cto=1e150, input values ~1e-150
	var A = new Float64Array( [ 1e-150, 1e-150, 2e-150, 3e-150 ] );
	var info = base( 'G', 0, 0, 1e-150, 1e150, 2, 1, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 'zlascl_large_ratio_inv a' );
});

test( 'zlascl: lower band matrix (type B)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_lower_band'; } );
	// Band storage: kl+1=2 rows by N=3 columns (interleaved complex)
	// Row 1 (diagonal): (1,0) (2,0) (3,0)
	// Row 2 (sub-diag):  (4,0) (5,0) (0,0)
	var A = new Float64Array( [
		1, 0,  4, 0,   // col 0: rows 0,1
		2, 0,  5, 0,   // col 1: rows 0,1
		3, 0,  0, 0    // col 2: rows 0,1
	] );
	var info = base( 'B', 1, 1, 1.0, 3.0, 3, 3, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 'zlascl_lower_band a' );
});

test( 'zlascl: upper band matrix (type Q)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_upper_band'; } );
	// Band storage: ku+1=2 rows by N=3 columns (interleaved complex)
	// Row 1 (super-diag): (0,0) (1,0) (2,0)
	// Row 2 (diagonal):   (3,0) (4,0) (5,0)
	var A = new Float64Array( [
		0, 0,  3, 0,   // col 0: rows 0,1
		1, 0,  4, 0,   // col 1: rows 0,1
		2, 0,  5, 0    // col 2: rows 0,1
	] );
	var info = base( 'Q', 1, 1, 1.0, 3.0, 3, 3, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 'zlascl_upper_band a' );
});

test( 'zlascl: full band matrix (type Z)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlascl_band'; } );
	// Band storage: 2*kl+ku+1=4 rows by N=3 columns (interleaved complex)
	// kl=1, ku=1
	// Using LDA=4, col-major with strideA1=1, strideA2=4
	var A = new Float64Array( [
		// col 0: rows 0,1,2,3
		0, 0,  3, 0,  6, 0,  9, 0,
		// col 1: rows 0,1,2,3
		1, 0,  4, 0,  7, 0,  10, 0,
		// col 2: rows 0,1,2,3
		2, 0,  5, 0,  8, 0,  0, 0
	] );
	var info = base( 'Z', 1, 1, 1.0, 2.0, 3, 3, A, 1, 4, 0 );
	assert.strictEqual( info, tc.info );
	var result = extractCMatrix( A, 1, 4, 0, 4, 3 );
	assertArrayClose( result, tc.a, 'zlascl_band a' );
});

test( 'zlascl: invalid type returns -1', function t() {
	var A = new Float64Array( [ 1, 2, 3, 4 ] );
	var info = base( 'X', 0, 0, 1.0, 2.0, 1, 1, A, 1, 1, 0 );
	assert.strictEqual( info, -1 );
});
