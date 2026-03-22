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
var zcopy = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zcopy.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zcopy: main export is a function', function t() {
	assert.strictEqual( typeof zcopy, 'function' );
});

test( 'zcopy: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zcopy.ndarray, 'function' );
});

test( 'zcopy: basic copy (N=3, strideX=1, strideY=1)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zcopy_basic'; } );
	var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	var zy = new Float64Array( 6 );
	var result = base( 3, zx, 1, 0, zy, 1, 0 );
	assert.strictEqual( result, zy );
	assertArrayClose( Array.from( zx ), tc.zx, 'zx unchanged' );
	assertArrayClose( Array.from( zy ), tc.zy, 'zy copied' );
});

test( 'zcopy: N=0 is a no-op', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zcopy_n_zero'; } );
	var zx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var zy = new Float64Array( [ 99.0, 88.0, 77.0, 66.0 ] );
	var result = base( 0, zx, 1, 0, zy, 1, 0 );
	assert.strictEqual( result, zy );
	assertArrayClose( Array.from( zy ), tc.zy, 'zy unchanged' );
});

test( 'zcopy: non-unit stride (strideX=2, strideY=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zcopy_stride'; } );
	var zx = new Float64Array( [
		1.0, 2.0, 99.0, 99.0, 3.0, 4.0, 99.0, 99.0, 5.0, 6.0
	] );
	var zy = new Float64Array( [
		77.0, 77.0, 88.0, 88.0, 77.0, 77.0, 88.0, 88.0, 77.0, 77.0
	] );
	var result = base( 3, zx, 2, 0, zy, 2, 0 );
	assert.strictEqual( result, zy );
	assertArrayClose( Array.from( zx ), tc.zx, 'zx unchanged' );
	assertArrayClose( Array.from( zy ), tc.zy, 'zy copied at stride' );
});

test( 'zcopy: mixed strides (strideX=1, strideY=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zcopy_mixed_stride'; } );
	var zx = new Float64Array( [
		10.0, 20.0, 30.0, 40.0, 50.0, 60.0
	] );
	var zy = new Float64Array( [
		0.0, 0.0, 88.0, 88.0, 0.0, 0.0, 88.0, 88.0, 0.0, 0.0
	] );
	var result = base( 3, zx, 1, 0, zy, 2, 0 );
	assert.strictEqual( result, zy );
	assertArrayClose( Array.from( zx ), tc.zx, 'zx unchanged' );
	assertArrayClose( Array.from( zy ), tc.zy, 'zy copied with mixed stride' );
});
