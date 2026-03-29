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
var Float64Array = require( '@stdlib/array/float64' );
var daxpy = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'daxpy.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'daxpy: main export is a function', function t() {
	assert.strictEqual( typeof daxpy, 'function' );
});

test( 'daxpy: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof daxpy.ndarray, 'function' );
});

test( 'daxpy: basic (N=5, alpha=2, incx=1, incy=1)', function t() {
	var result = base( 5, 2.0, x, 1, 0, y, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'basic'; } );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var y = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( y ), tc.dy, 'basic' );
});

test( 'daxpy: alpha=0 is a no-op', function t() {
	var result = base( 5, 0.0, x, 1, 0, y, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'da_zero'; } );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var y = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( y ), tc.dy, 'da_zero' );
});

test( 'daxpy: N=0 is a no-op', function t() {
	var result = base( 0, 2.0, x, 1, 0, y, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'n_zero'; } );
	var y = new Float64Array( [ 10.0, 20.0, 30.0, 40.0, 50.0 ] );
	var x = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( y ), tc.dy, 'n_zero' );
});

test( 'daxpy: N=1', function t() {
	var result = base( 1, 5.0, x, 1, 0, y, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'n_one'; } );
	var x = new Float64Array( [ 3.0 ] );
	var y = new Float64Array( [ 7.0 ] );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( y ), tc.dy, 'n_one' );
});

test( 'daxpy: non-unit strides (incx=2, incy=3)', function t() {
	var result = base( 3, 2.0, x, 2, 0, y, 3, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'stride'; } );
	var x = new Float64Array( [ 1.0, 0.0, 2.0, 0.0, 3.0 ] );
	var y = new Float64Array( [ 10.0, 0.0, 0.0, 20.0, 0.0, 0.0, 30.0, 0.0, 0.0 ] );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( y ), tc.dy, 'stride' );
});

test( 'daxpy: negative incx', function t() {
	var result = base( 3, 1.0, x, -1, 2, y, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'neg_incx'; } );
	var x = new Float64Array( [ 1.0, 2.0, 3.0 ] );
	var y = new Float64Array( [ 10.0, 20.0, 30.0 ] );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( y ), tc.dy, 'neg_incx' );
});

test( 'daxpy: unrolled loop (N=10, alpha=3, incx=1, incy=1)', function t() {
	var result = base( 10, 3.0, x, 1, 0, y, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'unrolled'; } );
	var x = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] );
	var y = new Float64Array( [ 10, 20, 30, 40, 50, 60, 70, 80, 90, 100 ] );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( y ), tc.dy, 'unrolled' );
});
