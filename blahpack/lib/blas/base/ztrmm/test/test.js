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
var ztrmm = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztrmm.jsonl' ), 'utf8' ).trim().split( '\n' );
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
function extractCMatrix( arr, M, N, sb1, sb2, offsetB ) {
	var out = [];
	var ib;
	var i;
	var j;
	var v;
	v = reinterpret( arr, 0 );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			ib = offsetB * 2 + 2 * ( i * sb1 + j * sb2 );
			out.push( v[ ib ] );
			out.push( v[ ib + 1 ] );
		}
	}
	return out;
}

// TESTS //

test( 'ztrmm: main export is a function', function t() {
	assert.strictEqual( typeof ztrmm, 'function' );
});

test( 'ztrmm: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof ztrmm.ndarray, 'function' );
});

test( 'ztrmm: left, upper, no transpose, non-unit (2x2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_left_upper_notrans'; } );
	// A upper 2x2 col-major: [2+1i, 0, 3+1i, 4+2i]
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	// B 2x2 col-major: [1+0i, 0+1i, 0+1i, 1+0i]
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'left', 'upper', 'no-transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: left, lower, no transpose, non-unit (2x2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_left_lower_notrans'; } );
	var A = new Complex128Array( [ 2, 1, 3, 1, 0, 0, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'left', 'lower', 'no-transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: right, upper, no transpose, non-unit (2x2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_right_upper_notrans'; } );
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'right', 'upper', 'no-transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: left, upper, conjugate transpose, non-unit (2x2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_left_upper_conjtrans'; } );
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'left', 'upper', 'conjugate-transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: alpha=0 zeros B', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_alpha_zero'; } );
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 0, 0 );
	var result = base( 'left', 'upper', 'no-transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: complex alpha', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_complex_alpha'; } );
	// A = identity (upper)
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 2, 0, 0, 2 ] );
	var alpha = new Complex128( 0, 1 );
	var result = base( 'left', 'upper', 'no-transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: M=0 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_m_zero'; } );
	var A = new Complex128Array( [ 1, 0 ] );
	var B = new Complex128Array( [ 99, 99 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'left', 'upper', 'no-transpose', 'non-unit', 0, 2, alpha, A, 1, 1, 0, B, 1, 1, 0 );
	assert.strictEqual( result, B );
	// B should be unchanged
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.b, 'b' );
});

test( 'ztrmm: unit diagonal, left, upper', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_unit_diag'; } );
	var A = new Complex128Array( [ 99, 99, 0, 0, 2, 1, 99, 99 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'left', 'upper', 'no-transpose', 'unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: left, upper, transpose (not conjugate), non-unit (2x2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_left_upper_trans'; } );
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'left', 'upper', 'transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: right, lower, no transpose, non-unit (2x2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_right_lower_notrans'; } );
	var A = new Complex128Array( [ 2, 1, 3, 1, 0, 0, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'right', 'lower', 'no-transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: left, lower, transpose, non-unit (2x2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_left_lower_trans'; } );
	var A = new Complex128Array( [ 2, 1, 3, 1, 0, 0, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'left', 'lower', 'transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: left, lower, conjugate transpose, non-unit (2x2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_left_lower_conjtrans'; } );
	var A = new Complex128Array( [ 2, 1, 3, 1, 0, 0, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'left', 'lower', 'conjugate-transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: right, upper, transpose, non-unit (2x2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_right_upper_trans'; } );
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'right', 'upper', 'transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: right, upper, conjugate transpose, non-unit (2x2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_right_upper_conjtrans'; } );
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'right', 'upper', 'conjugate-transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: right, lower, transpose, non-unit (2x2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_right_lower_trans'; } );
	var A = new Complex128Array( [ 2, 1, 3, 1, 0, 0, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'right', 'lower', 'transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrmm: right, lower, conjugate transpose, non-unit (2x2)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ztrmm_right_lower_conjtrans'; } );
	var A = new Complex128Array( [ 2, 1, 3, 1, 0, 0, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	var alpha = new Complex128( 1, 0 );
	var result = base( 'right', 'lower', 'conjugate-transpose', 'non-unit', 2, 2, alpha, A, 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
	assertArrayClose( extractCMatrix( B, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

// NDARRAY VALIDATION TESTS //

var ndarray = require( './../lib/ndarray.js' );

test( 'ndarray: throws TypeError for invalid side', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	assert.throws( function f() {
		ndarray( 'invalid', 'upper', 'no-transpose', 'non-unit', 2, 2, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid uplo', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	assert.throws( function f() {
		ndarray( 'left', 'invalid', 'no-transpose', 'non-unit', 2, 2, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid transa', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	assert.throws( function f() {
		ndarray( 'left', 'upper', 'invalid', 'non-unit', 2, 2, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid diag', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	assert.throws( function f() {
		ndarray( 'left', 'upper', 'no-transpose', 'invalid', 2, 2, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0 );
	}, TypeError );
});

test( 'ndarray: throws RangeError for negative M', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	assert.throws( function f() {
		ndarray( 'left', 'upper', 'no-transpose', 'non-unit', -1, 2, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0 );
	}, RangeError );
});

test( 'ndarray: throws RangeError for negative N', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
	assert.throws( function f() {
		ndarray( 'left', 'upper', 'no-transpose', 'non-unit', 2, -1, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0 );
	}, RangeError );
});
