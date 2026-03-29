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
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zaxpy = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zaxpy.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zaxpy: main export is a function', function t() {
	assert.strictEqual( typeof zaxpy, 'function' );
});

test( 'zaxpy: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zaxpy.ndarray, 'function' );
});

test( 'zaxpy: basic (N=3, za=(2,3), strideX=1, strideY=1)', function t() {
	var result = base( 3, za, zx, 1, 0, zy, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'zaxpy_basic'; } );
	var za = new Complex128( 2.0, 3.0 );
	var zx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	var zy = new Complex128Array( [ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0 ] );
	assert.strictEqual( result, zy );
	assertArrayClose( Array.from( reinterpret( zy, 0 ) ), tc.zy, 'zaxpy_basic zy' );

	// X should be unchanged
	assertArrayClose( Array.from( reinterpret( zx, 0 ) ), tc.zx, 'zaxpy_basic zx' );
});

test( 'zaxpy: alpha=0 is a no-op', function t() {
	var result = base( 2, za, zx, 1, 0, zy, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'zaxpy_alpha_zero'; } );
	var za = new Complex128( 0.0, 0.0 );
	var zx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var zy = new Complex128Array( [ 10.0, 20.0, 30.0, 40.0 ] );
	assert.strictEqual( result, zy );
	assertArrayClose( Array.from( reinterpret( zy, 0 ) ), tc.zy, 'zaxpy_alpha_zero' );
});

test( 'zaxpy: alpha=(1,0) adds x to y', function t() {
	var tc = fixture.find( function ( t ) { return t.name === 'zaxpy_alpha_one'; } );
	var za = new Complex128( 1.0, 0.0 );
	var zx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	var zy = new Complex128Array( [ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0 ] );
	base( 3, za, zx, 1, 0, zy, 1, 0 );
	assertArrayClose( Array.from( reinterpret( zy, 0 ) ), tc.zy, 'zaxpy_alpha_one' );
});

test( 'zaxpy: N=0 is a no-op', function t() {
	var result = base( 0, za, zx, 1, 0, zy, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'zaxpy_n_zero'; } );
	var za = new Complex128( 5.0, 6.0 );
	var zx = new Complex128Array( [ 1.0, 2.0 ] );
	var zy = new Complex128Array( [ 10.0, 20.0 ] );
	assert.strictEqual( result, zy );
	assertArrayClose( Array.from( reinterpret( zy, 0 ) ), tc.zy, 'zaxpy_n_zero' );
});

test( 'zaxpy: N=1', function t() {
	var tc = fixture.find( function ( t ) { return t.name === 'zaxpy_n_one'; } );
	var za = new Complex128( 1.0, 1.0 );
	var zx = new Complex128Array( [ 2.0, 3.0 ] );
	var zy = new Complex128Array( [ 4.0, 5.0 ] );
	base( 1, za, zx, 1, 0, zy, 1, 0 );
	assertArrayClose( Array.from( reinterpret( zy, 0 ) ), tc.zy, 'zaxpy_n_one' );
});

test( 'zaxpy: non-unit stride (strideX=2, strideY=2)', function t() {
	var result = base( 3, za, zx, 2, 0, zy, 2, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'zaxpy_stride'; } );
	var za = new Complex128( 2.0, 3.0 );
	var zx = new Complex128Array([
		1.0, 2.0, 99.0, 99.0, 3.0, 4.0, 99.0, 99.0, 5.0, 6.0
	]);
	var zy = new Complex128Array([
		10.0, 20.0, 88.0, 88.0, 30.0, 40.0, 88.0, 88.0, 50.0, 60.0
	]);
	assert.strictEqual( result, zy );
	assertArrayClose( Array.from( reinterpret( zy, 0 ) ), tc.zy, 'zaxpy_stride zy' );

	// X should be unchanged
	assertArrayClose( Array.from( reinterpret( zx, 0 ) ), tc.zx, 'zaxpy_stride zx' );
});

test( 'zaxpy: negative stride (strideX=-1 reverses x)', function t() {
	var tc = fixture.find( function ( t ) { return t.name === 'zaxpy_neg_stride'; } );

	// Fortran with incx=-1, n=3: ix = (-3+1)*(-1)+1 = 3, reads x(3), x(2), x(1)

	// In stdlib JS: strideX=-1, offsetX=2 (start at last element)
	var za = new Complex128( 1.0, 0.0 );
	var zx = new Complex128Array( [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 ] );
	var zy = new Complex128Array( [ 10.0, 20.0, 30.0, 40.0, 50.0, 60.0 ] );
	base( 3, za, zx, -1, 2, zy, 1, 0 );
	assertArrayClose( Array.from( reinterpret( zy, 0 ) ), tc.zy, 'zaxpy_neg_stride' );
});

test( 'zaxpy: purely imaginary alpha', function t() {
	var tc = fixture.find( function ( t ) { return t.name === 'zaxpy_imag_alpha'; } );
	var za = new Complex128( 0.0, 2.0 );
	var zx = new Complex128Array( [ 1.0, 0.0, 0.0, 1.0, 1.0, 1.0 ] );
	var zy = new Complex128Array( [ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ] );
	base( 3, za, zx, 1, 0, zy, 1, 0 );
	assertArrayClose( Array.from( reinterpret( zy, 0 ) ), tc.zy, 'zaxpy_imag_alpha' );
});

test( 'zaxpy: negative N is a no-op', function t() {
	var za = new Complex128( 2.0, 3.0 );
	var zx = new Complex128Array( [ 1.0, 2.0 ] );
	var zy = new Complex128Array( [ 10.0, 20.0 ] );
	var yv = reinterpret( zy, 0 );
	base( -1, za, zx, 1, 0, zy, 1, 0 );
	assert.strictEqual( yv[ 0 ], 10.0 );
	assert.strictEqual( yv[ 1 ], 20.0 );
});

test( 'zaxpy: offset support', function t() {
	// Use offsetX=1 to start from second element of x, offsetY=1 for y
	var za = new Complex128( 1.0, 0.0 );
	var zx = new Complex128Array( [ 99.0, 99.0, 1.0, 2.0, 3.0, 4.0 ] );
	var zy = new Complex128Array( [ 88.0, 88.0, 10.0, 20.0, 30.0, 40.0 ] );
	base( 2, za, zx, 1, 1, zy, 1, 1 );
	var yv = reinterpret( zy, 0 );

	// zy[0] should be unchanged
	assert.strictEqual( yv[ 0 ], 88.0 );
	assert.strictEqual( yv[ 1 ], 88.0 );

	// zy[1] = (10+20i) + (1+0i)*(1+2i) = (11+22i)
	assertClose( yv[ 2 ], 11.0, 'offset y[1] real' );
	assertClose( yv[ 3 ], 22.0, 'offset y[1] imag' );

	// zy[2] = (30+40i) + (1+0i)*(3+4i) = (33+44i)
	assertClose( yv[ 4 ], 33.0, 'offset y[2] real' );
	assertClose( yv[ 5 ], 44.0, 'offset y[2] imag' );
});
