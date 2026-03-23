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
var dpotf2 = require( './../lib' );
var dpotf2base = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );

// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpotf2.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		if ( expected[ i ] === 0.0 ) {
			assert.equal( actual[ i ], 0.0, msg + '[' + i + ']' );
		} else {
			relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.abs( expected[ i ] );
			assert.ok( relErr <= 1e-14, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
		}
	}
}


// TESTS //

test( 'dpotf2: main export is a function', function t() {
	assert.strictEqual( typeof dpotf2, 'function' );
});

test( 'dpotf2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dpotf2.ndarray, 'function' );
});

test( 'dpotf2: lower triangular 3x3 positive definite', function t() {
	var tc = findCase( 'lower_3x3' );
	// A = [4 2 1; 2 5 3; 1 3 6] stored column-major
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 6 ] );
	var info = dpotf2base( 'lower', 3, A, 1, 3, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( A, tc.a, 'lower_3x3' );
});

test( 'dpotf2: upper triangular 3x3 positive definite', function t() {
	var tc = findCase( 'upper_3x3' );
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 6 ] );
	var info = dpotf2base( 'upper', 3, A, 1, 3, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( A, tc.a, 'upper_3x3' );
});

test( 'dpotf2: n=1 matrix', function t() {
	var tc = findCase( 'n_one' );
	var A = new Float64Array( [ 9 ] );
	var info = dpotf2base( 'lower', 1, A, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( A, tc.a, 'n_one' );
});

test( 'dpotf2: n=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 0 );
	var info = dpotf2base( 'lower', 0, A, 1, 1, 0 );
	assert.strictEqual( info, tc.info );
});

test( 'dpotf2: not positive definite returns info > 0', function t() {
	var tc = findCase( 'not_posdef' );
	// A = [1 2; 2 1] col-major — 2x2 not positive definite
	var A = new Float64Array( [ 1, 2, 2, 1 ] );
	var info = dpotf2base( 'lower', 2, A, 1, 2, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( A, tc.a, 'not_posdef' );
});

test( 'dpotf2: upper triangle, not positive definite', function t() {
	// A = [1 2; 2 1] col-major — not positive definite, using 'U'
	// Upper: ajj = A[1,1] - dot(A[0:0,1], A[0:0,1]) = 1 - 4 = -3 → info=2
	var A = new Float64Array( [ 1, 2, 2, 1 ] );
	var info = dpotf2base( 'upper', 2, A, 1, 2, 0 );
	assert.strictEqual( info, 2, 'should return info=2' );
});

test( 'dpotf2: 4x4 identity matrix', function t() {
	var tc = findCase( 'identity_4x4' );
	// 4x4 identity in column-major
	var A = new Float64Array( [
		1, 0, 0, 0,
		0, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 1
	] );
	var info = dpotf2base( 'lower', 4, A, 1, 4, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( A, tc.a, 'identity_4x4' );
});

test( 'dpotf2: lower with offsetA > 0', function t() {
	// Put the 3x3 matrix at offset 2 in a larger buffer
	var tc = findCase( 'lower_3x3' );
	var A = new Float64Array( [ 99, 99, 4, 2, 1, 2, 5, 3, 1, 3, 6 ] );
	var info = dpotf2base( 'lower', 3, A, 1, 3, 2 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( A.subarray( 2 ), tc.a, 'lower_3x3_offset' );
});

test( 'dpotf2: upper with lowercase u', function t() {
	var tc = findCase( 'upper_3x3' );
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 6 ] );
	var info = dpotf2base( 'upper', 3, A, 1, 3, 0 );
	assert.strictEqual( info, tc.info );
	assertArrayClose( A, tc.a, 'upper_3x3_lowercase' );
});

// ndarray validation tests

test( 'dpotf2: ndarray throws TypeError for invalid uplo', function t() {
	assert.throws( function() {
		ndarray( 'invalid', 3, new Float64Array( 9 ), 1, 3, 0 );
	}, TypeError );
});

test( 'dpotf2: ndarray throws RangeError for negative N', function t() {
	assert.throws( function() {
		ndarray( 'upper', -1, new Float64Array( 9 ), 1, 3, 0 );
	}, RangeError );
});
