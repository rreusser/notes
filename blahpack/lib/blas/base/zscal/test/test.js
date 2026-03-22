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
var zscal = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zscal.jsonl' ), 'utf8' ).trim().split( '\n' );
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

// TESTS //

test( 'zscal: main export is a function', function t() {
	assert.strictEqual( typeof zscal, 'function' );
});

test( 'zscal: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zscal.ndarray, 'function' );
});

test( 'zscal: basic scale (N=3, za=(2,3), strideX=1)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zscal_basic'; } );
	var za = new Float64Array( [ 2.0, 3.0 ] );
	var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	var result = base( 3, za, zx, 1, 0 );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( zx ), tc.zx, 'zscal_basic' );
});

test( 'zscal: N=0 is a no-op', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zscal_n_zero'; } );
	var za = new Float64Array( [ 5.0, 6.0 ] );
	var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var result = base( 0, za, zx, 1, 0 );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( zx ), tc.zx, 'zscal_n_zero' );
});

test( 'zscal: za=(0,0) zeros out vector', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zscal_za_zero'; } );
	var za = new Float64Array( [ 0.0, 0.0 ] );
	var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	var result = base( 3, za, zx, 1, 0 );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( zx ), tc.zx, 'zscal_za_zero' );
});

test( 'zscal: non-unit stride (strideX=2, za=(0,1))', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zscal_stride'; } );
	var za = new Float64Array( [ 0.0, 1.0 ] );
	var zx = new Float64Array( [
		1.0, 2.0, 99.0, 99.0, 3.0, 4.0, 99.0, 99.0, 5.0, 6.0
	] );
	var result = base( 3, za, zx, 2, 0 );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( zx ), tc.zx, 'zscal_stride' );
});

test( 'zscal: za=(1,0) is identity (no-op)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zscal_za_one'; } );
	var za = new Float64Array( [ 1.0, 0.0 ] );
	var zx = new Float64Array( [ 7.0, 8.0, 9.0, 10.0 ] );
	var result = base( 2, za, zx, 1, 0 );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( zx ), tc.zx, 'zscal_za_one' );
});
