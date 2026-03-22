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
var zlacgv = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlacgv.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// HELPERS //

function assertArrayClose( actual, expected, msg ) {
	var relErr;
	var i;
	assert.strictEqual( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		if ( expected[ i ] === 0.0 ) {
			// For zero, check absolute: allow -0 to match 0
			assert.ok( Math.abs( actual[ i ] ) <= 1e-14, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
		} else {
			relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
			assert.ok( relErr <= 1e-14, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
		}
	}
}

// TESTS //

test( 'zlacgv: main export is a function', function t() {
	assert.strictEqual( typeof zlacgv, 'function' );
});

test( 'zlacgv: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlacgv.ndarray, 'function' );
});

test( 'zlacgv: basic conjugation (N=3, stride=1)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlacgv_basic'; } );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, -4.0, 5.0, 0.0 ] );
	base( 3, x, 1, 0 );
	assertArrayClose( Array.from( x ), tc.x, 'zlacgv_basic' );
});

test( 'zlacgv: N=0 is a no-op', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlacgv_n_zero'; } );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	base( 0, x, 1, 0 );
	assertArrayClose( Array.from( x ), tc.x, 'zlacgv_n_zero' );
});

test( 'zlacgv: N=1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlacgv_n_one'; } );
	var x = new Float64Array( [ 7.0, -3.0 ] );
	base( 1, x, 1, 0 );
	assertArrayClose( Array.from( x ), tc.x, 'zlacgv_n_one' );
});

test( 'zlacgv: non-unit stride (stride=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlacgv_stride2'; } );
	var x = new Float64Array( [
		1.0, 2.0, 99.0, 99.0, 3.0, 4.0, 99.0, 99.0, 5.0, 6.0
	] );
	base( 3, x, 2, 0 );
	assertArrayClose( Array.from( x ), tc.x, 'zlacgv_stride2' );
});

test( 'zlacgv: negative stride (stride=-1)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlacgv_neg_stride'; } );
	// With stride=-1, in Fortran IOFF = 1-(3-1)*(-1) = 3 (1-based)
	// In JS 0-based: offset = (3-1)*1*2 = 4 (pointing to last complex element)
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	base( 3, x, -1, 4 );
	assertArrayClose( Array.from( x ), tc.x, 'zlacgv_neg_stride' );
});

test( 'zlacgv: all zeros', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlacgv_zeros'; } );
	var x = new Float64Array( [ 0.0, 0.0, 0.0, 0.0 ] );
	base( 2, x, 1, 0 );
	assertArrayClose( Array.from( x ), tc.x, 'zlacgv_zeros' );
});

test( 'zlacgv: pure imaginary', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zlacgv_pure_imag'; } );
	var x = new Float64Array( [ 0.0, 5.0, 0.0, -3.0 ] );
	base( 2, x, 1, 0 );
	assertArrayClose( Array.from( x ), tc.x, 'zlacgv_pure_imag' );
});
