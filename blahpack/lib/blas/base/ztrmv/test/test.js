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
var ztrmv = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztrmv.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// HELPERS //

function assertClose( actual, expected, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= 1e-14, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, msg ) {
	var i;
	assert.strictEqual( actual.length, expected.length, msg + ': length mismatch (' + actual.length + ' vs ' + expected.length + ')' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], msg + '[' + i + ']' );
	}
}

// TESTS //

test( 'ztrmv: main export is a function', function t() {
	assert.strictEqual( typeof ztrmv, 'function' );
});

test( 'ztrmv: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof ztrmv.ndarray, 'function' );
});

test( 'ztrmv: upper triangular, no transpose, non-unit diagonal (N=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmv_upper_no_trans'; } );
	// A upper: A(0,0)=2+1i, A(0,1)=3+1i, A(1,1)=4+2i (col-major)
	// With strideA1=1, strideA2=2 for 2x2:
	// A flat (col-major): [2+1i, 0, 3+1i, 4+2i]
	var A = new Float64Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var x = new Float64Array( [ 1, 0, 1, 1 ] );
	var result = base( 'U', 'N', 'N', 2, A, 1, 2, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( x ), tc.x, 'x' );
});

test( 'ztrmv: lower triangular, no transpose, non-unit diagonal (N=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmv_lower_no_trans'; } );
	// A lower: A(0,0)=2+1i, A(1,0)=3+1i, A(1,1)=4+2i
	var A = new Float64Array( [ 2, 1, 3, 1, 0, 0, 4, 2 ] );
	var x = new Float64Array( [ 1, 0, 1, 1 ] );
	var result = base( 'L', 'N', 'N', 2, A, 1, 2, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( x ), tc.x, 'x' );
});

test( 'ztrmv: upper triangular, unit diagonal (N=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmv_unit_diag'; } );
	// Diagonal elements should be ignored (set to 99+99i)
	var A = new Float64Array( [ 99, 99, 0, 0, 3, 1, 99, 99 ] );
	var x = new Float64Array( [ 1, 0, 1, 1 ] );
	var result = base( 'U', 'N', 'U', 2, A, 1, 2, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( x ), tc.x, 'x' );
});

test( 'ztrmv: upper triangular, transpose (A^T), non-unit (N=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmv_upper_trans'; } );
	var A = new Float64Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var x = new Float64Array( [ 1, 0, 1, 1 ] );
	var result = base( 'U', 'T', 'N', 2, A, 1, 2, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( x ), tc.x, 'x' );
});

test( 'ztrmv: upper triangular, conjugate transpose (A^H), non-unit (N=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmv_upper_conjtrans'; } );
	var A = new Float64Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var x = new Float64Array( [ 1, 0, 1, 1 ] );
	var result = base( 'U', 'C', 'N', 2, A, 1, 2, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( x ), tc.x, 'x' );
});

test( 'ztrmv: N=0 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmv_n_zero'; } );
	var A = new Float64Array( [ 1, 0 ] );
	var x = new Float64Array( [ 5, 5 ] );
	var result = base( 'U', 'N', 'N', 0, A, 1, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( x ), tc.x, 'x' );
});

test( 'ztrmv: N=1, upper, non-unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmv_n_one'; } );
	var A = new Float64Array( [ 3, 2 ] );
	var x = new Float64Array( [ 2, 1 ] );
	var result = base( 'U', 'N', 'N', 1, A, 1, 1, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( x ), tc.x, 'x' );
});

test( 'ztrmv: non-unit stride (strideX=2), upper, no transpose (N=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmv_stride'; } );
	var A = new Float64Array( [ 2, 0, 0, 0, 1, 1, 3, 0 ] );
	// x: elem0 at index 0 = (1,0), elem1 at index 4 = (0,1)
	var x = new Float64Array( [ 1, 0, 99, 99, 0, 1 ] );
	var result = base( 'U', 'N', 'N', 2, A, 1, 2, 0, x, 2, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( x ), tc.x, 'x' );
});

test( 'ztrmv: lower, conjugate transpose, non-unit (N=3)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmv_lower_conjtrans'; } );
	// A lower 3x3 col-major:
	// col0: A(0,0)=1+1i, A(1,0)=2+1i, A(2,0)=3+1i
	// col1: A(0,1)=0,    A(1,1)=4+2i, A(2,1)=5+2i
	// col2: A(0,2)=0,    A(1,2)=0,    A(2,2)=6+3i
	var A = new Float64Array( [
		1, 1, 2, 1, 3, 1,
		0, 0, 4, 2, 5, 2,
		0, 0, 0, 0, 6, 3
	] );
	var x = new Float64Array( [ 1, 0, 0, 1, 1, 1 ] );
	var result = base( 'L', 'C', 'N', 3, A, 1, 3, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( x ), tc.x, 'x' );
});

test( 'ztrmv: lower, transpose (no conjugate), non-unit (N=3)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmv_lower_trans'; } );
	// Same lower 3x3 matrix as above, but trans='T' (no conjugate)
	var A = new Float64Array( [
		1, 1, 2, 1, 3, 1,
		0, 0, 4, 2, 5, 2,
		0, 0, 0, 0, 6, 3
	] );
	var x = new Float64Array( [ 1, 0, 0, 1, 1, 1 ] );
	var result = base( 'L', 'T', 'N', 3, A, 1, 3, 0, x, 1, 0 );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( x ), tc.x, 'x' );
});
