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
var ilazlc = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ilazlc.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// Fortran returns 1-based; JS returns 0-based. Convert:
function expected( tc ) {
	return tc.result - 1;
}

// For column-major complex matrix with LDA rows:
// strideA1 = 2 (each row occupies 2 doubles: re, im)
// strideA2 = 2 * LDA (each column occupies 2*LDA doubles)

// TESTS //

test( 'ilazlc: main export is a function', function t() {
	assert.strictEqual( typeof ilazlc, 'function' );
});

test( 'ilazlc: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof ilazlc.ndarray, 'function' );
});

test( 'ilazlc: diagonal 3x3 matrix -> last non-zero column = 2 (0-based)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_diag'; } );
	// LDA=4, 3x3 matrix. Column-major interleaved: strideA1=2, strideA2=2*4=8
	var A = new Float64Array( 2 * 4 * 3 ); // 4 rows * 3 cols * 2 (re,im)
	A[ 0 * 2 + 0 * 8 ] = 1.0; // (0,0) real
	A[ 1 * 2 + 1 * 8 ] = 2.0; // (1,1) real
	A[ 2 * 2 + 2 * 8 ] = 3.0; // (2,2) real
	assert.strictEqual( base( 3, 3, A, 2, 8, 0 ), expected( tc ) );
});

test( 'ilazlc: last column all zeros -> returns 1 (0-based col 2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_col2'; } );
	var A = new Float64Array( 2 * 4 * 3 );
	A[ 0 * 2 + 0 * 8 ] = 1.0; // (0,0) real
	A[ 1 * 2 + 1 * 8 ] = 2.0; // (1,1) real
	assert.strictEqual( base( 3, 3, A, 2, 8, 0 ), expected( tc ) );
});

test( 'ilazlc: all zeros -> returns -1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_zeros'; } );
	var A = new Float64Array( 2 * 4 * 3 );
	assert.strictEqual( base( 3, 3, A, 2, 8, 0 ), expected( tc ) );
});

test( 'ilazlc: N=0 -> returns -1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_n_zero'; } );
	var A = new Float64Array( 2 * 4 * 3 );
	assert.strictEqual( base( 3, 0, A, 2, 8, 0 ), expected( tc ) );
});

test( 'ilazlc: only imaginary part non-zero in last column', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_imag'; } );
	var A = new Float64Array( 2 * 4 * 3 );
	// a(2, 3) = (0, 5i) -> row 1 (0-based), col 2 (0-based)
	A[ 1 * 2 + 2 * 8 + 1 ] = 5.0; // imaginary part
	assert.strictEqual( base( 3, 3, A, 2, 8, 0 ), expected( tc ) );
});

test( 'ilazlc: full matrix -> returns N-1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_full'; } );
	var A = new Float64Array( 2 * 4 * 3 );
	var vals = [
		[1,1], [2,2], [3,3],
		[4,4], [5,5], [6,6],
		[7,7], [8,8], [9,9]
	];
	var i, j, k;
	k = 0;
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			A[ i * 2 + j * 8 ] = vals[ k ][ 0 ];
			A[ i * 2 + j * 8 + 1 ] = vals[ k ][ 1 ];
			k++;
		}
	}
	assert.strictEqual( base( 3, 3, A, 2, 8, 0 ), expected( tc ) );
});

test( 'ilazlc: 1x1 non-zero -> returns 0', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_1x1'; } );
	var A = new Float64Array( 2 * 4 );
	A[ 0 ] = 1.0;
	assert.strictEqual( base( 1, 1, A, 2, 8, 0 ), expected( tc ) );
});

test( 'ilazlc: 1x1 zero -> returns -1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilazlc_1x1_zero'; } );
	var A = new Float64Array( 2 * 4 );
	assert.strictEqual( base( 1, 1, A, 2, 8, 0 ), expected( tc ) );
});
