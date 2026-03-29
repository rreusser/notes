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
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgerc = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgerc.jsonl' ), 'utf8' ).trim().split( '\n' );
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

/**
* Extract M x N complex matrix from interleaved flat array with given strides.
* Returns just the M*N complex values (2*M*N doubles) in column-major order.
*/
function extractCMatrix( arr, M, N, sa1, sa2, offsetA ) {
	var out = [];
	var ia;
	var i;
	var j;
	var v;
	v = reinterpret( arr, 0 );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			ia = offsetA * 2 + 2 * ( i * sa1 + j * sa2 );
			out.push( v[ ia ] );
			out.push( v[ ia + 1 ] );
		}
	}
	return out;
}

// TESTS //

test( 'zgerc: main export is a function', function t() {
	assert.strictEqual( typeof zgerc, 'function' );
});

test( 'zgerc: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgerc.ndarray, 'function' );
});

test( 'zgerc: basic 2x2 rank-1 update', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgerc_basic'; } );
	// A is 2x2 col-major: A(0,0)=1+1i, A(1,0)=2+2i, A(0,1)=3+3i, A(1,1)=4+4i
	var A = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var x = new Complex128Array( [ 1, 0, 0, 1 ] );
	var y = new Complex128Array( [ 1, 1, 0, 2 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 2, 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( extractCMatrix( A, 2, 2, 1, 2, 0 ), tc.a, 'a' );
});

test( 'zgerc: n=0 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgerc_n_zero'; } );
	var A = new Complex128Array( [ 1, 1, 2, 2 ] );
	var x = new Complex128Array( [ 5, 5 ] );
	var y = new Complex128Array( [ 6, 6 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 2, 0, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 'a' );
});

test( 'zgerc: m=0 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgerc_m_zero'; } );
	var A = new Complex128Array( [ 1, 1, 2, 2 ] );
	var x = new Complex128Array( [ 5, 5 ] );
	var y = new Complex128Array( [ 6, 6, 7, 7 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 0, 2, alpha, x, 1, 0, y, 1, 0, A, 1, 0, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 'a' );
});

test( 'zgerc: alpha=0 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgerc_alpha_zero'; } );
	var A = new Complex128Array( [ 7, 7, 8, 8, 9, 9, 10, 10 ] );
	var x = new Complex128Array( [ 5, 5, 6, 6 ] );
	var y = new Complex128Array( [ 7, 7, 8, 8 ] );
	var alpha = new Complex128( 0, 0 );
	var result = base( 2, 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( extractCMatrix( A, 2, 2, 1, 2, 0 ), tc.a, 'a' );
});

test( 'zgerc: complex alpha', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgerc_complex_alpha'; } );
	// identity matrix
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var x = new Complex128Array( [ 1, 0, 0, 1 ] );
	var y = new Complex128Array( [ 1, 0, 0, 1 ] );
	var alpha = new Complex128( 0, 1 );
	var result = base( 2, 2, alpha, x, 1, 0, y, 1, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( extractCMatrix( A, 2, 2, 1, 2, 0 ), tc.a, 'a' );
});

test( 'zgerc: non-unit strides (strideX=2, strideY=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgerc_stride'; } );
	var A = new Complex128Array( [ 0, 0, 0, 0, 0, 0, 0, 0 ] );
	// x: elem0=(1,2) at offset 0, elem1=(3,4) at offset 2 (stride=2 complex elts)
	var x = new Complex128Array( [ 1, 2, 99, 99, 3, 4 ] );
	var y = new Complex128Array( [ 5, 6, 99, 99, 7, 8 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 2, 2, alpha, x, 2, 0, y, 2, 0, A, 1, 2, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( extractCMatrix( A, 2, 2, 1, 2, 0 ), tc.a, 'a' );
});

test( 'zgerc: 3x2 non-square', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgerc_nonsquare'; } );
	// 3x2 col-major: col0=[1+0i, 0, 0], col1=[0, 1+0i, 0]
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 ] );
	var x = new Complex128Array( [ 1, 0, 2, 0, 3, 0 ] );
	var y = new Complex128Array( [ 1, 1, 2, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 3, 2, alpha, x, 1, 0, y, 1, 0, A, 1, 3, 0 );
	assert.strictEqual( result, A );
	assertArrayClose( extractCMatrix( A, 3, 2, 1, 3, 0 ), tc.a, 'a' );
});
