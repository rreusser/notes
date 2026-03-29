/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

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
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zdscal = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zdscal.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zdscal: main export is a function', function t() {
	assert.strictEqual( typeof zdscal, 'function' );
});

test( 'zdscal: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zdscal.ndarray, 'function' );
});

test( 'zdscal: basic scale (N=3, da=2.0, strideX=1)', function t() {
	var result = base( 3, 2.0, zx, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'basic'; } );
	var zx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( reinterpret( zx, 0 ) ), tc.zx, 'basic' );
});

test( 'zdscal: da=0 zeros out vector', function t() {
	var result = base( 3, 0.0, zx, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'da_zero'; } );
	var zx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( reinterpret( zx, 0 ) ), tc.zx, 'da_zero' );
});

test( 'zdscal: da=1 is identity', function t() {
	var result = base( 3, 1.0, zx, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'da_one'; } );
	var zx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( reinterpret( zx, 0 ) ), tc.zx, 'da_one' );
});

test( 'zdscal: N=0 is a no-op', function t() {
	var result = base( 0, 5.0, zx, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'n_zero'; } );
	var zx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( reinterpret( zx, 0 ) ), tc.zx, 'n_zero' );
});

test( 'zdscal: N=1', function t() {
	var result = base( 1, 3.0, zx, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'n_one'; } );
	var zx = new Complex128Array( [ 7.0, 3.0 ] );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( reinterpret( zx, 0 ) ), tc.zx, 'n_one' );
});

test( 'zdscal: non-unit stride (strideX=2)', function t() {
	var result = base( 3, 4.0, zx, 2, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'stride'; } );
	var zx = new Complex128Array([
		1.0, 2.0, 99.0, 99.0, 3.0, 4.0, 99.0, 99.0, 5.0, 6.0
	]);
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( reinterpret( zx, 0 ) ), tc.zx, 'stride' );
});

test( 'zdscal: negative da', function t() {
	var result = base( 2, -2.0, zx, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'neg_da'; } );
	var zx = new Complex128Array( [ 2.0, 3.0, 4.0, 5.0 ] );
	assert.strictEqual( result, zx );
	assertArrayClose( Array.from( reinterpret( zx, 0 ) ), tc.zx, 'neg_da' );
});
