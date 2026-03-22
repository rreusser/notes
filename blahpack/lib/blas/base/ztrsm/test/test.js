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
var ztrsm = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztrsm.jsonl' ), 'utf8' ).trim().split( '\n' );
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

/**
* Create upper triangular 2x2 complex A: [(2,1), (3,1); 0, (4,2)] col-major.
*/
function upperA() {
	return new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
}

/**
* Create lower triangular 2x2 complex A: [(2,1), 0; (3,1), (4,2)] col-major.
*/
function lowerA() {
	return new Complex128Array( [ 2, 1, 3, 1, 0, 0, 4, 2 ] );
}

/**
* Create standard 2x2 complex B: [(1,0),(0,1); (0,1),(1,0)] col-major.
*/
function stdB() {
	return new Complex128Array( [ 1, 0, 0, 1, 0, 1, 1, 0 ] );
}

/**
* Create unit-diagonal upper A (diagonal ignored): col-major [(99,99),0; (3,1),(99,99)].
*/
function upperUnitA() {
	return new Complex128Array( [ 99, 99, 0, 0, 3, 1, 99, 99 ] );
}

/**
* Create unit-diagonal lower A (diagonal ignored): col-major [(99,99),(3,1); 0,(99,99)].
*/
function lowerUnitA() {
	return new Complex128Array( [ 99, 99, 3, 1, 0, 0, 99, 99 ] );
}

// TESTS //

test( 'ztrsm: main export is a function', function t() {
	assert.strictEqual( typeof ztrsm, 'function' );
});

test( 'ztrsm: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof ztrsm.ndarray, 'function' );
});

// ========================================================================
// 12 tests: all side x uplo x trans combinations, non-unit diagonal
// ========================================================================

test( 'ztrsm: left, upper, no-transpose, non-unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'left_upper_notrans_nonunit'; } );
	var result = base( 'L', 'U', 'N', 'N', 2, 2, new Complex128( 1, 0 ), upperA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: left, upper, transpose, non-unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'left_upper_trans_nonunit'; } );
	var result = base( 'L', 'U', 'T', 'N', 2, 2, new Complex128( 1, 0 ), upperA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: left, upper, conj-transpose, non-unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'left_upper_conjtrans_nonunit'; } );
	var result = base( 'L', 'U', 'C', 'N', 2, 2, new Complex128( 1, 0 ), upperA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: left, lower, no-transpose, non-unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'left_lower_notrans_nonunit'; } );
	var result = base( 'L', 'L', 'N', 'N', 2, 2, new Complex128( 1, 0 ), lowerA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: left, lower, transpose, non-unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'left_lower_trans_nonunit'; } );
	var result = base( 'L', 'L', 'T', 'N', 2, 2, new Complex128( 1, 0 ), lowerA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: left, lower, conj-transpose, non-unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'left_lower_conjtrans_nonunit'; } );
	var result = base( 'L', 'L', 'C', 'N', 2, 2, new Complex128( 1, 0 ), lowerA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, upper, no-transpose, non-unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_upper_notrans_nonunit'; } );
	var result = base( 'R', 'U', 'N', 'N', 2, 2, new Complex128( 1, 0 ), upperA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, upper, transpose, non-unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_upper_trans_nonunit'; } );
	var result = base( 'R', 'U', 'T', 'N', 2, 2, new Complex128( 1, 0 ), upperA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, upper, conj-transpose, non-unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_upper_conjtrans_nonunit'; } );
	var result = base( 'R', 'U', 'C', 'N', 2, 2, new Complex128( 1, 0 ), upperA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, lower, no-transpose, non-unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_lower_notrans_nonunit'; } );
	var result = base( 'R', 'L', 'N', 'N', 2, 2, new Complex128( 1, 0 ), lowerA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, lower, transpose, non-unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_lower_trans_nonunit'; } );
	var result = base( 'R', 'L', 'T', 'N', 2, 2, new Complex128( 1, 0 ), lowerA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, lower, conj-transpose, non-unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_lower_conjtrans_nonunit'; } );
	var result = base( 'R', 'L', 'C', 'N', 2, 2, new Complex128( 1, 0 ), lowerA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

// ========================================================================
// 12 tests: all side x uplo x trans combinations, unit diagonal
// ========================================================================

test( 'ztrsm: left, upper, no-transpose, unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'left_upper_notrans_unit'; } );
	var result = base( 'L', 'U', 'N', 'U', 2, 2, new Complex128( 1, 0 ), upperUnitA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: left, upper, transpose, unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'left_upper_trans_unit'; } );
	var result = base( 'L', 'U', 'T', 'U', 2, 2, new Complex128( 1, 0 ), upperUnitA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: left, upper, conj-transpose, unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'left_upper_conjtrans_unit'; } );
	var result = base( 'L', 'U', 'C', 'U', 2, 2, new Complex128( 1, 0 ), upperUnitA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: left, lower, no-transpose, unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'left_lower_notrans_unit'; } );
	var result = base( 'L', 'L', 'N', 'U', 2, 2, new Complex128( 1, 0 ), lowerUnitA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: left, lower, transpose, unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'left_lower_trans_unit'; } );
	var result = base( 'L', 'L', 'T', 'U', 2, 2, new Complex128( 1, 0 ), lowerUnitA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: left, lower, conj-transpose, unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'left_lower_conjtrans_unit'; } );
	var result = base( 'L', 'L', 'C', 'U', 2, 2, new Complex128( 1, 0 ), lowerUnitA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, upper, no-transpose, unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_upper_notrans_unit'; } );
	var result = base( 'R', 'U', 'N', 'U', 2, 2, new Complex128( 1, 0 ), upperUnitA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, upper, transpose, unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_upper_trans_unit'; } );
	var result = base( 'R', 'U', 'T', 'U', 2, 2, new Complex128( 1, 0 ), upperUnitA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, upper, conj-transpose, unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_upper_conjtrans_unit'; } );
	var result = base( 'R', 'U', 'C', 'U', 2, 2, new Complex128( 1, 0 ), upperUnitA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, lower, no-transpose, unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_lower_notrans_unit'; } );
	var result = base( 'R', 'L', 'N', 'U', 2, 2, new Complex128( 1, 0 ), lowerUnitA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, lower, transpose, unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_lower_trans_unit'; } );
	var result = base( 'R', 'L', 'T', 'U', 2, 2, new Complex128( 1, 0 ), lowerUnitA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, lower, conj-transpose, unit', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_lower_conjtrans_unit'; } );
	var result = base( 'R', 'L', 'C', 'U', 2, 2, new Complex128( 1, 0 ), lowerUnitA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

// ========================================================================
// Edge case tests
// ========================================================================

test( 'ztrsm: alpha=0 zeros B', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'alpha_zero'; } );
	var result = base( 'L', 'U', 'N', 'N', 2, 2, new Complex128( 0, 0 ), upperA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: complex alpha=(0,1) with identity A', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'complex_alpha'; } );
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 1, 0 ] );
	var B = new Complex128Array( [ 1, 0, 0, 1, 2, 0, 0, 2 ] );
	var result = base( 'L', 'U', 'N', 'N', 2, 2, new Complex128( 0, 1 ), A, 1, 2, 0, B, 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: complex alpha=(2,-1) with non-identity A', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'complex_alpha_nonidentity'; } );
	var result = base( 'L', 'U', 'N', 'N', 2, 2, new Complex128( 2, -1 ), upperA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: M=0 quick return (B unchanged)', function t() {
	var B = new Complex128Array( [ 99, 99 ] );
	var result = base( 'L', 'U', 'N', 'N', 0, 2, new Complex128( 1, 0 ), upperA(), 1, 2, 0, B, 1, 1, 0 );
	assert.strictEqual( result, B );
	var v = reinterpret( B, 0 );
	assert.strictEqual( v[ 0 ], 99 );
	assert.strictEqual( v[ 1 ], 99 );
});

test( 'ztrsm: N=0 quick return (B unchanged)', function t() {
	var B = new Complex128Array( [ 99, 99 ] );
	var result = base( 'L', 'U', 'N', 'N', 2, 0, new Complex128( 1, 0 ), upperA(), 1, 2, 0, B, 1, 1, 0 );
	assert.strictEqual( result, B );
	var v = reinterpret( B, 0 );
	assert.strictEqual( v[ 0 ], 99 );
	assert.strictEqual( v[ 1 ], 99 );
});

test( 'ztrsm: 3x2 left, upper, no-transpose', function t() {
	var tc = fixture.find( function( t ) { return t.name === '3x2_left_upper_notrans'; } );
	// A 3x3 upper: [(1,1), (2,0), (0,1); 0, (3,-1), (1,2); 0, 0, (2,0)] col-major
	var A = new Complex128Array( [
		1, 1, 0, 0, 0, 0,   // col 0
		2, 0, 3, -1, 0, 0,  // col 1
		0, 1, 1, 2, 2, 0    // col 2
	] );
	var B = new Complex128Array( [
		1, 0, 0, 1, 2, -1,   // col 0
		-1, 1, 3, 0, 0, 0    // col 1
	] );
	var result = base( 'L', 'U', 'N', 'N', 3, 2, new Complex128( 1, 0 ), A, 1, 3, 0, B, 1, 3, 0 );
	assertArrayClose( extractCMatrix( result, 3, 2, 1, 3, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, upper, conj-transpose with complex alpha', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_upper_conjtrans_alpha'; } );
	var result = base( 'R', 'U', 'C', 'N', 2, 2, new Complex128( 2, -1 ), upperA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: right, lower, transpose with complex alpha', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'right_lower_trans_alpha'; } );
	var result = base( 'R', 'L', 'T', 'N', 2, 2, new Complex128( 2, -1 ), lowerA(), 1, 2, 0, stdB(), 1, 2, 0 );
	assertArrayClose( extractCMatrix( result, 2, 2, 1, 2, 0 ), tc.b, 'b' );
});

test( 'ztrsm: returns the B array', function t() {
	var B = stdB();
	var result = base( 'L', 'U', 'N', 'N', 2, 2, new Complex128( 1, 0 ), upperA(), 1, 2, 0, B, 1, 2, 0 );
	assert.strictEqual( result, B );
});
