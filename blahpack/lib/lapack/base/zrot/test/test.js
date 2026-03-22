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
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zrot.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zrot: main export is a function', function t() {
	assert.strictEqual( typeof base, 'function' );
});

test( 'zrot: basic rotation c=1/sqrt(2), s=(1/sqrt(2),0)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zrot_basic'; } );
	var c = 1.0 / Math.sqrt( 2.0 );
	var s = new Float64Array( [ 1.0 / Math.sqrt( 2.0 ), 0.0 ] );
	var cx = new Float64Array( [ 1.0, 0.0, 0.0, 1.0, 1.0, 1.0 ] );
	var cy = new Float64Array( [ 0.0, 0.0, 1.0, 0.0, 0.0, -1.0 ] );
	base( 3, cx, 1, 0, cy, 1, 0, c, s );
	assertArrayClose( Array.from( cx ), tc.cx, 'zrot_basic cx' );
	assertArrayClose( Array.from( cy ), tc.cy, 'zrot_basic cy' );
});

test( 'zrot: n=0 is a no-op', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zrot_n_zero'; } );
	var c = 1.0 / Math.sqrt( 2.0 );
	var s = new Float64Array( [ 1.0 / Math.sqrt( 2.0 ), 0.0 ] );
	var cx = new Float64Array( [ 1.0, 2.0 ] );
	var cy = new Float64Array( [ 3.0, 4.0 ] );
	base( 0, cx, 1, 0, cy, 1, 0, c, s );
	assertArrayClose( Array.from( cx ), tc.cx, 'zrot_n_zero cx' );
	assertArrayClose( Array.from( cy ), tc.cy, 'zrot_n_zero cy' );
});

test( 'zrot: identity rotation c=1, s=(0,0)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zrot_identity'; } );
	var c = 1.0;
	var s = new Float64Array( [ 0.0, 0.0 ] );
	var cx = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var cy = new Float64Array( [ 5.0, 6.0, 7.0, 8.0 ] );
	base( 2, cx, 1, 0, cy, 1, 0, c, s );
	assertArrayClose( Array.from( cx ), tc.cx, 'zrot_identity cx' );
	assertArrayClose( Array.from( cy ), tc.cy, 'zrot_identity cy' );
});

test( 'zrot: complex s, c=0.6, s=(0,0.8)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zrot_complex_s'; } );
	var c = 0.6;
	var s = new Float64Array( [ 0.0, 0.8 ] );
	var cx = new Float64Array( [ 1.0, 0.0, 0.0, 1.0 ] );
	var cy = new Float64Array( [ 0.0, 1.0, 1.0, 0.0 ] );
	base( 2, cx, 1, 0, cy, 1, 0, c, s );
	assertArrayClose( Array.from( cx ), tc.cx, 'zrot_complex_s cx' );
	assertArrayClose( Array.from( cy ), tc.cy, 'zrot_complex_s cy' );
});

test( 'zrot: non-unit stride (strideX=2, strideY=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zrot_stride'; } );
	var c = 0.0;
	var s = new Float64Array( [ 1.0, 0.0 ] );
	var cx = new Float64Array( [ 1.0, 0.0, 99.0, 99.0, 2.0, 0.0 ] );
	var cy = new Float64Array( [ 3.0, 0.0, 99.0, 99.0, 4.0, 0.0 ] );
	base( 2, cx, 2, 0, cy, 2, 0, c, s );
	assertArrayClose( Array.from( cx ), tc.cx, 'zrot_stride cx' );
	assertArrayClose( Array.from( cy ), tc.cy, 'zrot_stride cy' );
});
