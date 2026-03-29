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
var ztrmv = require( './../lib' );
var base = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'ztrmv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

// TESTS //

test( 'ztrmv: main export is a function', function t() {
	assert.strictEqual( typeof ztrmv, 'function' );
});

test( 'ztrmv: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof ztrmv.ndarray, 'function' );
});

test( 'ztrmv: upper triangular, no transpose, non-unit diagonal (N=2)', function t() {
	var result = base( 'upper', 'no-transpose', 'non-unit', 2, A, 1, 2, 0, x, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'ztrmv_upper_no_trans'; } );
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztrmv: lower triangular, no transpose, non-unit diagonal (N=2)', function t() {
	var result = base( 'lower', 'no-transpose', 'non-unit', 2, A, 1, 2, 0, x, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'ztrmv_lower_no_trans'; } );
	var A = new Complex128Array( [ 2, 1, 3, 1, 0, 0, 4, 2 ] );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztrmv: upper triangular, unit diagonal (N=2)', function t() {
	var result = base( 'upper', 'no-transpose', 'unit', 2, A, 1, 2, 0, x, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'ztrmv_unit_diag'; } );
	var A = new Complex128Array( [ 99, 99, 0, 0, 3, 1, 99, 99 ] );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztrmv: upper triangular, transpose (A^T), non-unit (N=2)', function t() {
	var result = base( 'upper', 'transpose', 'non-unit', 2, A, 1, 2, 0, x, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'ztrmv_upper_trans'; } );
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztrmv: upper triangular, conjugate transpose (A^H), non-unit (N=2)', function t() {
	var result = base( 'upper', 'conjugate-transpose', 'non-unit', 2, A, 1, 2, 0, x, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'ztrmv_upper_conjtrans'; } );
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztrmv: N=0 quick return', function t() {
	var result = base( 'upper', 'no-transpose', 'non-unit', 0, A, 1, 1, 0, x, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'ztrmv_n_zero'; } );
	var A = new Complex128Array( [ 1, 0 ] );
	var x = new Complex128Array( [ 5, 5 ] );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztrmv: N=1, upper, non-unit', function t() {
	var result = base( 'upper', 'no-transpose', 'non-unit', 1, A, 1, 1, 0, x, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'ztrmv_n_one'; } );
	var A = new Complex128Array( [ 3, 2 ] );
	var x = new Complex128Array( [ 2, 1 ] );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztrmv: non-unit stride (strideX=2), upper, no transpose (N=2)', function t() {
	var result = base( 'upper', 'no-transpose', 'non-unit', 2, A, 1, 2, 0, x, 2, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'ztrmv_stride'; } );
	var A = new Complex128Array( [ 2, 0, 0, 0, 1, 1, 3, 0 ] );
	var x = new Complex128Array( [ 1, 0, 99, 99, 0, 1 ] );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztrmv: lower, conjugate transpose, non-unit (N=3)', function t() {
	var result = base( 'lower', 'conjugate-transpose', 'non-unit', 3, A, 1, 3, 0, x, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'ztrmv_lower_conjtrans'; } );
	var A = new Complex128Array([
		1,
		1,
		2,
		1,
		3,
		1,
		0,
		0,
		4,
		2,
		5,
		2,
		0,
		0,
		0,
		0,
		6,
		3
	]);
	var x = new Complex128Array( [ 1, 0, 0, 1, 1, 1 ] );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});

test( 'ztrmv: lower, transpose (no conjugate), non-unit (N=3)', function t() {
	var result = base( 'lower', 'transpose', 'non-unit', 3, A, 1, 3, 0, x, 1, 0 );
	var tc = fixture.find( function ( t ) { return t.name === 'ztrmv_lower_trans'; } );
	var A = new Complex128Array([
		1,
		1,
		2,
		1,
		3,
		1,
		0,
		0,
		4,
		2,
		5,
		2,
		0,
		0,
		0,
		0,
		6,
		3
	]);
	var x = new Complex128Array( [ 1, 0, 0, 1, 1, 1 ] );
	assert.strictEqual( result, x );
	assertArrayClose( Array.from( reinterpret( x, 0 ) ), tc.x, 'x' );
});


// NDARRAY VALIDATION TESTS //

test( 'ndarray: throws TypeError for invalid uplo', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'foo', 'no-transpose', 'non-unit', 2, A, 1, 2, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid trans', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'foo', 'non-unit', 2, A, 1, 2, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid diag', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'foo', 2, A, 1, 2, 0, x, 1, 0 );
	}, TypeError );
});

test( 'ndarray: throws RangeError for negative N', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', -1, A, 1, 2, 0, x, 1, 0 );
	}, RangeError );
});

test( 'ndarray: throws RangeError for strideX=0', function t() {
	var A = new Complex128Array( [ 2, 1, 0, 0, 3, 1, 4, 2 ] );
	var x = new Complex128Array( [ 1, 0, 1, 1 ] );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 'non-unit', 2, A, 1, 2, 0, x, 0, 0 );
	}, RangeError );
});

test( 'ndarray: N=0 early return', function t() {
	var out = ndarray( 'upper', 'no-transpose', 'non-unit', 0, A, 1, 1, 0, x, 1, 0 );
	var xv = reinterpret( x, 0 );
	var A = new Complex128Array( [ 1, 0 ] );
	var x = new Complex128Array( [ 5, 5 ] );
	assert.strictEqual( xv[ 0 ], 5 );
	assert.strictEqual( xv[ 1 ], 5 );
});
