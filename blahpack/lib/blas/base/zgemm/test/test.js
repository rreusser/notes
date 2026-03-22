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
var zgemm = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgemm.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zgemm: main export is a function', function t() {
	assert.strictEqual( typeof zgemm, 'function' );
});

test( 'zgemm: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgemm.ndarray, 'function' );
});

test( 'zgemm: basic N,N (M=2, N=2, K=2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_basic_nn'; } );
	// A col-major 2x2: [(1,1), (2,0), (3,0), (4,1)]
	var A = new Complex128Array( [ 1, 1, 2, 0, 3, 0, 4, 1 ] );
	// B col-major 2x2: [(1,0), (0,-1), (0,1), (1,0)]
	var B = new Complex128Array( [ 1, 0, 0, -1, 0, 1, 1, 0 ] );
	var C = new Complex128Array( 4 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'N', 'N', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_basic_nn c' );
});

test( 'zgemm: conjugate transpose A (C=A^H*B)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_conjA'; } );
	var A = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 1, 1, 2, 0 ] );
	var C = new Complex128Array( 4 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'C', 'N', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_conjA c' );
});

test( 'zgemm: alpha and beta scaling', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_alpha_beta'; } );
	// A = I (identity)
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var B = new Complex128Array( [ 2, 1, 0, 0, 0, 0, 3, 2 ] );
	var C = new Complex128Array( [ 1, 1, 0, 0, 0, 0, 1, 1 ] );
	var alpha = new Complex128( 2, 1 );
	var beta = new Complex128( 1, -1 );
	var result = base( 'N', 'N', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_alpha_beta c' );
});

test( 'zgemm: M=0 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_m_zero'; } );
	var A = new Complex128Array( 0 );
	var B = new Complex128Array( 0 );
	var C = new Complex128Array( [ 99, 88 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 1, 0 );
	var result = base( 'N', 'N', 0, 2, 2, alpha, A, 1, 1, 0, B, 1, 1, 0, beta, C, 1, 1, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_m_zero c' );
});

test( 'zgemm: transpose B (C=A*B^T)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_transB'; } );
	var A = new Complex128Array( [ 1, 1, 2, 0, 0, 1, 1, -1 ] );
	var B = new Complex128Array( [ 1, 0, 0, 0, 0, 1, 1, 0 ] );
	var C = new Complex128Array( 4 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'N', 'T', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_transB c' );
});

test( 'zgemm: conjugate transpose B (C=A*B^H)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_conjB'; } );
	var A = new Complex128Array( [ 1, 0, 0, 1, 2, 0, 0, 2 ] );
	var B = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var C = new Complex128Array( 4 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'N', 'C', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_conjB c' );
});

test( 'zgemm: transpose A, no transpose B (C=A^T*B)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_transA_N'; } );
	var A = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 1, 1, 2, 0 ] );
	var C = new Complex128Array( 4 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'T', 'N', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_transA_N c' );
});

test( 'zgemm: alpha=0, beta=0 (C should be zeroed)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_alpha_zero_beta_zero'; } );
	var A = new Complex128Array( [ 1, 1, 2, 0, 0, 0, 0, 0 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 0, 0, 0 ] );
	var C = new Complex128Array( [ 5, 3, 7, 2, 1, 1, 9, 4 ] );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'N', 'N', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_alpha_zero_beta_zero c' );
});

test( 'zgemm: alpha=0, non-trivial beta (C := beta*C)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_alpha_zero_beta_scale'; } );
	var A = new Complex128Array( [ 1, 1, 0, 0, 0, 0, 0, 0 ] );
	var B = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0 ] );
	var C = new Complex128Array( [ 2, 1, 3, 2, 4, 3, 5, 4 ] );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 2, 1 );
	var result = base( 'N', 'N', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_alpha_zero_beta_scale c' );
});

test( 'zgemm: alpha=0, beta=1 quick return (C unchanged)', function t() {
	var A = new Complex128Array( [ 1, 1, 0, 0, 0, 0, 0, 0 ] );
	var B = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0 ] );
	var C = new Complex128Array( [ 2, 1, 3, 2, 4, 3, 5, 4 ] );
	var alpha = new Complex128( 0, 0 );
	var beta = new Complex128( 1, 0 );
	var result = base( 'N', 'N', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assert.deepStrictEqual( Array.from( reinterpret( C, 0 ) ), [ 2, 1, 3, 2, 4, 3, 5, 4 ] );
});

test( 'zgemm: conjugate A, conjugate B (C=A^H*B^H)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_conjA_conjB'; } );
	var A = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var B = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var C = new Complex128Array( 4 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'C', 'C', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_conjA_conjB c' );
});

test( 'zgemm: transpose A, transpose B (C=A^T*B^T)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_transA_transB'; } );
	var A = new Complex128Array( [ 1, 1, 2, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 2, 1, 1, -1 ] );
	var C = new Complex128Array( 4 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'T', 'T', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_transA_transB c' );
});

test( 'zgemm: transpose A, conjugate B (C=A^T*B^H)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_transA_conjB'; } );
	var A = new Complex128Array( [ 1, 1, 2, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var C = new Complex128Array( 4 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'T', 'C', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_transA_conjB c' );
});

test( 'zgemm: conjugate A, transpose B (C=A^H*B^T)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_conjA_transB'; } );
	var A = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 2, 1, 1, -1 ] );
	var C = new Complex128Array( 4 );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0, 0 );
	var result = base( 'C', 'T', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_conjA_transB c' );
});

test( 'zgemm: K=0 (C := beta*C)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_k_zero'; } );
	var A = new Complex128Array( 0 );
	var B = new Complex128Array( 0 );
	var C = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 2, 0 );
	var result = base( 'N', 'N', 2, 2, 0, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_k_zero c' );
});

test( 'zgemm: N,T with non-trivial beta', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_NT_beta_scale'; } );
	// A = I, B = I
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var B = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var C = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 2, 1 );
	var result = base( 'N', 'T', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_NT_beta_scale c' );
});

test( 'zgemm: C,C with non-zero beta', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_CC_beta_nonzero'; } );
	// A = I, B = I
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var B = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var C = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0.5, 0.5 );
	var result = base( 'C', 'C', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_CC_beta_nonzero c' );
});

test( 'zgemm: N,C with non-trivial beta', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_NC_beta_scale'; } );
	// A = I
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var B = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var C = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 2, 0 );
	var result = base( 'N', 'C', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_NC_beta_scale c' );
});

test( 'zgemm: T,N with non-zero beta', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_TN_beta_nonzero'; } );
	// A = I
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 1, 1, 2, 0 ] );
	var C = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 1, 1 );
	var result = base( 'T', 'N', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_TN_beta_nonzero c' );
});

test( 'zgemm: C,N with non-zero beta', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_CN_beta_nonzero'; } );
	// A = I
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 1, 1, 2, 0 ] );
	var C = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 1, 1 );
	var result = base( 'C', 'N', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_CN_beta_nonzero c' );
});

test( 'zgemm: C,T with non-zero beta', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_CT_beta_nonzero'; } );
	// A = I
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 2, 1, 1, -1 ] );
	var C = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0.5, 0 );
	var result = base( 'C', 'T', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_CT_beta_nonzero c' );
});

test( 'zgemm: T,C with non-zero beta', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_TC_beta_nonzero'; } );
	// A = I
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var B = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var C = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0.5, 0 );
	var result = base( 'T', 'C', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_TC_beta_nonzero c' );
});

test( 'zgemm: T,T with non-zero beta', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'zgemm_TT_beta_nonzero'; } );
	// A = I
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 2, 1, 1, -1 ] );
	var C = new Complex128Array( [ 1, 1, 2, 2, 3, 3, 4, 4 ] );
	var alpha = new Complex128( 1, 0 );
	var beta = new Complex128( 0.5, 0 );
	var result = base( 'T', 'T', 2, 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0, beta, C, 1, 2, 0 );
	assert.strictEqual( result, C );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 'zgemm_TT_beta_nonzero c' );
});
