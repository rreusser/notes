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
var izamax = require( './../lib' );
var base = require( './../lib/base.js' );

// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'izamax.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// TESTS //

test( 'izamax: main export is a function', function t() {
	assert.strictEqual( typeof izamax, 'function' );
});

test( 'izamax: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof izamax.ndarray, 'function' );
});

test( 'izamax: basic (n=4, strideX=1)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'basic'; } );
	// zx = [(1,2), (5,1), (2,3), (4,0)] interleaved
	var zx = new Float64Array( [ 1, 2, 5, 1, 2, 3, 4, 0 ] );
	var result = base( 4, zx, 1, 0 );
	// Fortran returns 1-based; JS returns 0-based
	assert.strictEqual( result, tc.result - 1 );
});

test( 'izamax: n=0 returns -1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'n_zero'; } );
	var zx = new Float64Array( [ 1, 2 ] );
	var result = base( 0, zx, 1, 0 );
	// Fortran returns 0 for n<1; JS returns -1
	assert.strictEqual( result, -1 );
	// Verify Fortran returned 0
	assert.strictEqual( tc.result, 0 );
});

test( 'izamax: n=1 returns 0', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'n_one'; } );
	var zx = new Float64Array( [ 1, 2 ] );
	var result = base( 1, zx, 1, 0 );
	// Fortran returns 1 (1-based); JS returns 0 (0-based)
	assert.strictEqual( result, tc.result - 1 );
});

test( 'izamax: non-unit stride (strideX=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'stride'; } );
	// Fortran: zx(1)=(1,2), zx(2)=(99,99), zx(3)=(2,3), zx(4)=(99,99), zx(5)=(10,10)
	// With incx=2, accessed elements: zx(1), zx(3), zx(5)
	// Interleaved Float64Array (all 5 complex elements):
	var zx = new Float64Array( [
		1, 2,      // element 0
		99, 99,    // element 1
		2, 3,      // element 2
		99, 99,    // element 3
		10, 10     // element 4
	] );
	var result = base( 3, zx, 2, 0 );
	// Fortran returns 3 (1-based); JS returns 2 (0-based)
	assert.strictEqual( result, tc.result - 1 );
});

test( 'izamax: equal magnitudes returns first', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'equal'; } );
	// zx = [(3,2), (1,4), (5,0)] -- all have |Re|+|Im| = 5
	var zx = new Float64Array( [ 3, 2, 1, 4, 5, 0 ] );
	var result = base( 3, zx, 1, 0 );
	// Fortran returns 1 (1-based); JS returns 0 (0-based)
	assert.strictEqual( result, tc.result - 1 );
});

test( 'izamax: negative components', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'negative'; } );
	// zx = [(-3,-4), (1,1), (-2,5)]
	// |Re|+|Im|: 7, 2, 7 -- first max is element 0
	var zx = new Float64Array( [ -3, -4, 1, 1, -2, 5 ] );
	var result = base( 3, zx, 1, 0 );
	assert.strictEqual( result, tc.result - 1 );
});

test( 'izamax: offset support', function t() {
	// Test that offset works: skip first complex element
	// zx = [(0,0), (1,2), (5,1), (2,3), (4,0)]
	// With offset=2, we start at element index 1 (the (1,2) pair)
	var zx = new Float64Array( [ 0, 0, 1, 2, 5, 1, 2, 3, 4, 0 ] );
	var result = base( 4, zx, 1, 2 );
	// Same as 'basic' test, max is second element (0-based index 1)
	assert.strictEqual( result, 1 );
});
