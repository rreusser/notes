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
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgemv = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgemv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zgemv: main export is a function', function t() {
	assert.strictEqual( typeof zgemv, 'function' );
});

test( 'zgemv: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgemv.ndarray, 'function' );
});

test( 'zgemv: basic trans=N (M=2, N=2, alpha=(1,0), beta=(0,0))', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemv_basic'; } );
	// A column-major 2x2: [(1,1), (2,2), (3,3), (4,4)]
	// Interleaved: [1,1, 2,2, 3,3, 4,4]
	var A = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var x = new Complex128Array( [ 1, 0, 1, 0 ] );
	var y = new Complex128Array( 2 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	// Column-major: strideA1=1, strideA2=M=2
	var result = base( 'no-transpose', 2, 2, alpha, A, 1, 2, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'zgemv_basic y' );
});

test( 'zgemv: conjugate transpose (trans=C, M=2, N=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemv_conj_trans'; } );
	var A = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var x = new Complex128Array( [ 1, 1, 1, 1 ] );
	var y = new Complex128Array( 2 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'conjugate-transpose', 2, 2, alpha, A, 1, 2, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'zgemv_conj_trans y' );
});

test( 'zgemv: alpha and beta scaling (trans=N)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemv_alpha_beta'; } );
	// A = [(1,0), (0,1), (2,0), (0,2)] col-major 2x2
	var A = new Complex128Array( [ 1, 0, 0, 1, 2, 0, 0, 2 ] );
	var x = new Complex128Array( [ 1, 0, 1, 0 ] );
	var y = new Complex128Array( [ 1, 0, 0, 1 ] );
	var alpha = new Complex128( 2, 1 );
	var beta = new Complex128( 1, 1 );
	var result = base( 'no-transpose', 2, 2, alpha, A, 1, 2, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'zgemv_alpha_beta y' );
});

test( 'zgemv: zero dimensions (M=0, N=0) — quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemv_zero_dim'; } );
	var A = new Complex128Array( 0 );
	var x = new Complex128Array( 0 );
	var y = new Complex128Array( [ 99, 88 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'no-transpose', 0, 0, alpha, A, 1, 1, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'zgemv_zero_dim y' );
});

test( 'zgemv: transpose (trans=T, no conjugate)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemv_trans'; } );
	var A = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var x = new Complex128Array( [ 1, 0, 0, 1 ] );
	var y = new Complex128Array( 2 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'transpose', 2, 2, alpha, A, 1, 2, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'zgemv_trans y' );
});

test( 'zgemv: alpha=0, beta=1 quick return (y unchanged)', function t() {
	var A = new Complex128Array( [ 1, 0, 2, 0, 3, 0, 4, 0 ] );
	var x = new Complex128Array( [ 1, 1, 2, 2 ] );
	var y = new Complex128Array( [ 5, 6, 7, 8 ] );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 1, 0 );
	var result = base( 'no-transpose', 2, 2, alpha, A, 1, 2, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	assert.deepStrictEqual( Array.from( reinterpret( y, 0 ) ), [ 5, 6, 7, 8 ] );
});

test( 'zgemv: alpha=0, non-trivial beta (y := beta*y only)', function t() {
	var A = new Complex128Array( [ 1, 0, 2, 0, 3, 0, 4, 0 ] );
	var x = new Complex128Array( [ 1, 1, 2, 2 ] );
	var y = new Complex128Array( [ 1, 0, 0, 1 ] );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 2, 0 );
	var result = base( 'no-transpose', 2, 2, alpha, A, 1, 2, 0, x, 1, 0, beta, y, 1, 0 );
	assert.strictEqual( result, y );
	// y should be scaled by 2: [2, 0, 0, 2]
	assert.deepStrictEqual( Array.from( reinterpret( y, 0 ) ), [ 2, 0, 0, 2 ] );
});

test( 'zgemv: non-unit stride (incx=2, incy=2, trans=N)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemv_stride'; } );
	// A = [(1,0), (2,0), (3,0), (4,0)] col-major 2x2
	var A = new Complex128Array( [ 1, 0, 2, 0, 3, 0, 4, 0 ] );
	// x: element 0=(1,1), gap=(99,99), element 1=(2,2)
	var x = new Complex128Array( [ 1, 1, 99, 99, 2, 2 ] );
	// y: element 0=(0,0), gap=(88,88), element 1=(0,0)
	var y = new Complex128Array( [ 0, 0, 88, 88, 0, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'no-transpose', 2, 2, alpha, A, 1, 2, 0, x, 2, 0, beta, y, 2, 0 );
	assert.strictEqual( result, y );
	assertArrayClose( Array.from( reinterpret( y, 0 ) ), tc.y, 'zgemv_stride y' );
});
