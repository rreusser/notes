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
var zgghrd = require( './../lib' );
var base = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgghrd.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// HELPERS //

function assertArrayClose( actual, expected, msg ) {
	var relErr;
	var i;
	assert.strictEqual( actual.length, expected.length, msg + ': length mismatch (' + actual.length + ' vs ' + expected.length + ')' );
	for ( i = 0; i < expected.length; i++ ) {
		if ( expected[ i ] === 0.0 ) {
			assert.ok( Math.abs( actual[ i ] ) <= 1e-14, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
		} else {
			relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
			assert.ok( relErr <= 1e-14, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
		}
	}
}

/**
* Extract column j (0-based) from an interleaved complex matrix.
*
* @param {Float64Array} M - interleaved complex matrix
* @param {integer} j - column index (0-based)
* @param {integer} nrows - number of rows to extract
* @param {integer} LDA - leading dimension (number of allocated rows)
* @returns {Array} array of 2*nrows doubles [re0, im0, re1, im1, ...]
*/
function extractCol( M, j, nrows, LDA ) {
	var result = [];
	var start = j * 2 * LDA;
	var i;
	for ( i = 0; i < nrows; i++ ) {
		result.push( M[ start + i * 2 ] );
		result.push( M[ start + i * 2 + 1 ] );
	}
	return result;
}

/**
* Set complex element (i, j) in interleaved matrix.
*
* @param {Float64Array} M - matrix
* @param {integer} LDA - leading dimension
* @param {integer} i - row (0-based)
* @param {integer} j - col (0-based)
* @param {number} re - real part
* @param {number} im - imaginary part
*/
function cset( M, LDA, i, j, re, im ) {
	var idx = j * 2 * LDA + i * 2;
	M[ idx ] = re;
	M[ idx + 1 ] = im;
}

// TESTS //

test( 'zgghrd: main export is a function', function t() {
	assert.strictEqual( typeof zgghrd, 'function' );
});

test( 'zgghrd: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgghrd.ndarray, 'function' );
});

test( 'zgghrd: basic 4x4 with COMPQ=I, COMPZ=I', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'basic_4x4'; } );
	var n = 4;
	var LDA = n;
	var A = new Float64Array( 2 * LDA * n );
	var B = new Float64Array( 2 * LDA * n );
	var Q = new Float64Array( 2 * LDA * n );
	var Z = new Float64Array( 2 * LDA * n );
	var sa1 = 2;
	var sa2 = 2 * LDA;
	var info;

	// Initialize A
	cset( A, LDA, 0, 0, 2.0, 1.0 );
	cset( A, LDA, 0, 1, 1.0, 0.5 );
	cset( A, LDA, 0, 2, 0.5, -0.5 );
	cset( A, LDA, 0, 3, 1.0, 1.0 );
	cset( A, LDA, 1, 0, 1.0, -1.0 );
	cset( A, LDA, 1, 1, 3.0, 0.0 );
	cset( A, LDA, 1, 2, 1.0, 1.0 );
	cset( A, LDA, 1, 3, 0.5, 0.5 );
	cset( A, LDA, 2, 0, 0.5, 0.5 );
	cset( A, LDA, 2, 1, 2.0, -1.0 );
	cset( A, LDA, 2, 2, 4.0, 1.0 );
	cset( A, LDA, 2, 3, 1.0, 0.0 );
	cset( A, LDA, 3, 0, 0.0, 1.0 );
	cset( A, LDA, 3, 1, 1.0, 1.0 );
	cset( A, LDA, 3, 2, 0.5, -0.5 );
	cset( A, LDA, 3, 3, 2.0, -1.0 );

	// Initialize B (upper triangular)
	cset( B, LDA, 0, 0, 3.0, 0.0 );
	cset( B, LDA, 0, 1, 1.0, 0.5 );
	cset( B, LDA, 0, 2, 0.5, 0.5 );
	cset( B, LDA, 0, 3, 0.0, 1.0 );
	cset( B, LDA, 1, 1, 2.0, 1.0 );
	cset( B, LDA, 1, 2, 1.0, 0.0 );
	cset( B, LDA, 1, 3, 0.5, -0.5 );
	cset( B, LDA, 2, 2, 4.0, -1.0 );
	cset( B, LDA, 2, 3, 1.0, 1.0 );
	cset( B, LDA, 3, 3, 1.0, 0.0 );

	info = base( 'I', 'I', n, 1, 4, A, sa1, sa2, 0, B, sa1, sa2, 0, Q, sa1, sa2, 0, Z, sa1, sa2, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCol( A, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( A, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( A, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( A, 3, n, LDA ), tc.A_col4, 'A_col4' );
	assertArrayClose( extractCol( B, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( B, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( B, 2, n, LDA ), tc.B_col3, 'B_col3' );
	assertArrayClose( extractCol( B, 3, n, LDA ), tc.B_col4, 'B_col4' );
	assertArrayClose( extractCol( Q, 0, n, LDA ), tc.Q_col1, 'Q_col1' );
	assertArrayClose( extractCol( Q, 1, n, LDA ), tc.Q_col2, 'Q_col2' );
	assertArrayClose( extractCol( Q, 2, n, LDA ), tc.Q_col3, 'Q_col3' );
	assertArrayClose( extractCol( Q, 3, n, LDA ), tc.Q_col4, 'Q_col4' );
	assertArrayClose( extractCol( Z, 0, n, LDA ), tc.Z_col1, 'Z_col1' );
	assertArrayClose( extractCol( Z, 1, n, LDA ), tc.Z_col2, 'Z_col2' );
	assertArrayClose( extractCol( Z, 2, n, LDA ), tc.Z_col3, 'Z_col3' );
	assertArrayClose( extractCol( Z, 3, n, LDA ), tc.Z_col4, 'Z_col4' );
});

test( 'zgghrd: n=1 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'n_equals_1'; } );
	var n = 1;
	var LDA = 1;
	var A = new Float64Array( 2 );
	var B = new Float64Array( 2 );
	var Q = new Float64Array( 2 );
	var Z = new Float64Array( 2 );
	var sa1 = 2;
	var sa2 = 2;
	var info;

	A[ 0 ] = 5.0; A[ 1 ] = 3.0;
	B[ 0 ] = 2.0; B[ 1 ] = 1.0;

	info = base( 'I', 'I', n, 1, 1, A, sa1, sa2, 0, B, sa1, sa2, 0, Q, sa1, sa2, 0, Z, sa1, sa2, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( [ A[ 0 ], A[ 1 ] ], tc.A, 'A' );
	assertArrayClose( [ B[ 0 ], B[ 1 ] ], tc.B, 'B' );
	assertArrayClose( [ Q[ 0 ], Q[ 1 ] ], tc.Q, 'Q' );
	assertArrayClose( [ Z[ 0 ], Z[ 1 ] ], tc.Z, 'Z' );
});

test( 'zgghrd: no Q/Z accumulation (COMPQ=N, COMPZ=N) 3x3', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'no_qz_3x3'; } );
	var n = 3;
	var LDA = n;
	var A = new Float64Array( 2 * LDA * n );
	var B = new Float64Array( 2 * LDA * n );
	var Q = new Float64Array( 2 * LDA * n );
	var Z = new Float64Array( 2 * LDA * n );
	var sa1 = 2;
	var sa2 = 2 * LDA;
	var info;

	cset( A, LDA, 0, 0, 1.0, 0.0 );
	cset( A, LDA, 0, 1, 2.0, 1.0 );
	cset( A, LDA, 0, 2, 3.0, -1.0 );
	cset( A, LDA, 1, 0, 4.0, 2.0 );
	cset( A, LDA, 1, 1, 5.0, 0.0 );
	cset( A, LDA, 1, 2, 6.0, 1.0 );
	cset( A, LDA, 2, 0, 7.0, -1.0 );
	cset( A, LDA, 2, 1, 8.0, 2.0 );
	cset( A, LDA, 2, 2, 9.0, 0.0 );

	cset( B, LDA, 0, 0, 2.0, 0.0 );
	cset( B, LDA, 0, 1, 1.0, 1.0 );
	cset( B, LDA, 0, 2, 0.5, 0.0 );
	cset( B, LDA, 1, 1, 3.0, 0.0 );
	cset( B, LDA, 1, 2, 1.0, -1.0 );
	cset( B, LDA, 2, 2, 1.0, 0.0 );

	info = base( 'N', 'N', n, 1, 3, A, sa1, sa2, 0, B, sa1, sa2, 0, Q, sa1, sa2, 0, Z, sa1, sa2, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCol( A, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( A, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( A, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( B, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( B, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( B, 2, n, LDA ), tc.B_col3, 'B_col3' );
});

test( 'zgghrd: partial reduction ILO=2, IHI=4 on 5x5', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'partial_5x5'; } );
	var n = 5;
	var LDA = n;
	var A = new Float64Array( 2 * LDA * n );
	var B = new Float64Array( 2 * LDA * n );
	var Q = new Float64Array( 2 * LDA * n );
	var Z = new Float64Array( 2 * LDA * n );
	var sa1 = 2;
	var sa2 = 2 * LDA;
	var info;
	var i;
	var j;

	// Fill A with pattern: A(i,j) = (i+j+2, (i-j)*0.5)
	for ( j = 0; j < n; j++ ) {
		for ( i = 0; i < n; i++ ) {
			cset( A, LDA, i, j, ( i + 1 ) + ( j + 1 ), ( ( i + 1 ) - ( j + 1 ) ) * 0.5 );
		}
	}

	// B upper triangular: B(i,j) = (i+j+2, (j-i)*0.25) for i <= j
	for ( j = 0; j < n; j++ ) {
		for ( i = 0; i <= j; i++ ) {
			cset( B, LDA, i, j, ( i + 1 ) + ( j + 1 ), ( ( j + 1 ) - ( i + 1 ) ) * 0.25 );
		}
	}

	info = base( 'I', 'I', n, 2, 4, A, sa1, sa2, 0, B, sa1, sa2, 0, Q, sa1, sa2, 0, Z, sa1, sa2, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCol( A, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( A, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( A, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( A, 3, n, LDA ), tc.A_col4, 'A_col4' );
	assertArrayClose( extractCol( A, 4, n, LDA ), tc.A_col5, 'A_col5' );
	assertArrayClose( extractCol( B, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( B, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( B, 2, n, LDA ), tc.B_col3, 'B_col3' );
	assertArrayClose( extractCol( B, 3, n, LDA ), tc.B_col4, 'B_col4' );
	assertArrayClose( extractCol( B, 4, n, LDA ), tc.B_col5, 'B_col5' );
	assertArrayClose( extractCol( Q, 0, n, LDA ), tc.Q_col1, 'Q_col1' );
	assertArrayClose( extractCol( Q, 1, n, LDA ), tc.Q_col2, 'Q_col2' );
	assertArrayClose( extractCol( Q, 2, n, LDA ), tc.Q_col3, 'Q_col3' );
	assertArrayClose( extractCol( Q, 3, n, LDA ), tc.Q_col4, 'Q_col4' );
	assertArrayClose( extractCol( Q, 4, n, LDA ), tc.Q_col5, 'Q_col5' );
	assertArrayClose( extractCol( Z, 0, n, LDA ), tc.Z_col1, 'Z_col1' );
	assertArrayClose( extractCol( Z, 1, n, LDA ), tc.Z_col2, 'Z_col2' );
	assertArrayClose( extractCol( Z, 2, n, LDA ), tc.Z_col3, 'Z_col3' );
	assertArrayClose( extractCol( Z, 3, n, LDA ), tc.Z_col4, 'Z_col4' );
	assertArrayClose( extractCol( Z, 4, n, LDA ), tc.Z_col5, 'Z_col5' );
});

test( 'zgghrd: accumulate into existing Q, Z (COMPQ=V, COMPZ=V) 3x3', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'accumulate_3x3'; } );
	var n = 3;
	var LDA = n;
	var A = new Float64Array( 2 * LDA * n );
	var B = new Float64Array( 2 * LDA * n );
	var Q = new Float64Array( 2 * LDA * n );
	var Z = new Float64Array( 2 * LDA * n );
	var sa1 = 2;
	var sa2 = 2 * LDA;
	var info;
	var i;

	cset( A, LDA, 0, 0, 2.0, 0.0 );
	cset( A, LDA, 0, 1, 1.0, 1.0 );
	cset( A, LDA, 0, 2, 0.0, 1.0 );
	cset( A, LDA, 1, 0, 3.0, -1.0 );
	cset( A, LDA, 1, 1, 1.0, 0.0 );
	cset( A, LDA, 1, 2, 2.0, 0.5 );
	cset( A, LDA, 2, 0, 1.0, 2.0 );
	cset( A, LDA, 2, 1, 0.0, -1.0 );
	cset( A, LDA, 2, 2, 3.0, 1.0 );

	cset( B, LDA, 0, 0, 1.0, 0.0 );
	cset( B, LDA, 0, 1, 0.5, 0.5 );
	cset( B, LDA, 0, 2, 0.0, 1.0 );
	cset( B, LDA, 1, 1, 2.0, 0.0 );
	cset( B, LDA, 1, 2, 1.0, 0.0 );
	cset( B, LDA, 2, 2, 3.0, 0.0 );

	// Initialize Q and Z to identity
	for ( i = 0; i < n; i++ ) {
		cset( Q, LDA, i, i, 1.0, 0.0 );
		cset( Z, LDA, i, i, 1.0, 0.0 );
	}

	info = base( 'V', 'V', n, 1, 3, A, sa1, sa2, 0, B, sa1, sa2, 0, Q, sa1, sa2, 0, Z, sa1, sa2, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCol( A, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( A, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( A, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( B, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( B, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( B, 2, n, LDA ), tc.B_col3, 'B_col3' );
	assertArrayClose( extractCol( Q, 0, n, LDA ), tc.Q_col1, 'Q_col1' );
	assertArrayClose( extractCol( Q, 1, n, LDA ), tc.Q_col2, 'Q_col2' );
	assertArrayClose( extractCol( Q, 2, n, LDA ), tc.Q_col3, 'Q_col3' );
	assertArrayClose( extractCol( Z, 0, n, LDA ), tc.Z_col1, 'Z_col1' );
	assertArrayClose( extractCol( Z, 1, n, LDA ), tc.Z_col2, 'Z_col2' );
	assertArrayClose( extractCol( Z, 2, n, LDA ), tc.Z_col3, 'Z_col3' );
});

test( 'zgghrd: ILO=IHI (already reduced, near no-op)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilo_eq_ihi'; } );
	var n = 3;
	var LDA = n;
	var A = new Float64Array( 2 * LDA * n );
	var B = new Float64Array( 2 * LDA * n );
	var Q = new Float64Array( 2 * LDA * n );
	var Z = new Float64Array( 2 * LDA * n );
	var sa1 = 2;
	var sa2 = 2 * LDA;
	var info;

	cset( A, LDA, 0, 0, 1.0, 0.0 );
	cset( A, LDA, 0, 1, 2.0, 0.0 );
	cset( A, LDA, 0, 2, 3.0, 0.0 );
	cset( A, LDA, 1, 0, 4.0, 0.0 );
	cset( A, LDA, 1, 1, 5.0, 0.0 );
	cset( A, LDA, 1, 2, 6.0, 0.0 );
	cset( A, LDA, 2, 0, 7.0, 0.0 );
	cset( A, LDA, 2, 1, 8.0, 0.0 );
	cset( A, LDA, 2, 2, 9.0, 0.0 );

	cset( B, LDA, 0, 0, 1.0, 0.0 );
	cset( B, LDA, 0, 1, 1.0, 0.0 );
	cset( B, LDA, 0, 2, 1.0, 0.0 );
	cset( B, LDA, 1, 1, 2.0, 0.0 );
	cset( B, LDA, 1, 2, 1.0, 0.0 );
	cset( B, LDA, 2, 2, 3.0, 0.0 );

	info = base( 'I', 'I', n, 2, 2, A, sa1, sa2, 0, B, sa1, sa2, 0, Q, sa1, sa2, 0, Z, sa1, sa2, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCol( A, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( A, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( A, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( B, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( B, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( B, 2, n, LDA ), tc.B_col3, 'B_col3' );
	assertArrayClose( extractCol( Q, 0, n, LDA ), tc.Q_col1, 'Q_col1' );
	assertArrayClose( extractCol( Q, 1, n, LDA ), tc.Q_col2, 'Q_col2' );
	assertArrayClose( extractCol( Q, 2, n, LDA ), tc.Q_col3, 'Q_col3' );
	assertArrayClose( extractCol( Z, 0, n, LDA ), tc.Z_col1, 'Z_col1' );
	assertArrayClose( extractCol( Z, 1, n, LDA ), tc.Z_col2, 'Z_col2' );
	assertArrayClose( extractCol( Z, 2, n, LDA ), tc.Z_col3, 'Z_col3' );
});

test( 'zgghrd: empty range IHI=ILO-1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'empty_range'; } );
	var n = 3;
	var LDA = n;
	var A = new Float64Array( 2 * LDA * n );
	var B = new Float64Array( 2 * LDA * n );
	var Q = new Float64Array( 2 * LDA * n );
	var Z = new Float64Array( 2 * LDA * n );
	var sa1 = 2;
	var sa2 = 2 * LDA;
	var info;

	cset( A, LDA, 0, 0, 1.0, 0.0 );
	cset( A, LDA, 0, 1, 2.0, 0.0 );
	cset( A, LDA, 0, 2, 3.0, 0.0 );
	cset( A, LDA, 1, 0, 4.0, 0.0 );
	cset( A, LDA, 1, 1, 5.0, 0.0 );
	cset( A, LDA, 1, 2, 6.0, 0.0 );
	cset( A, LDA, 2, 0, 7.0, 0.0 );
	cset( A, LDA, 2, 1, 8.0, 0.0 );
	cset( A, LDA, 2, 2, 9.0, 0.0 );

	cset( B, LDA, 0, 0, 1.0, 0.0 );
	cset( B, LDA, 1, 1, 2.0, 0.0 );
	cset( B, LDA, 2, 2, 3.0, 0.0 );

	info = base( 'I', 'I', n, 2, 1, A, sa1, sa2, 0, B, sa1, sa2, 0, Q, sa1, sa2, 0, Z, sa1, sa2, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCol( A, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( A, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( A, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( B, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( B, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( B, 2, n, LDA ), tc.B_col3, 'B_col3' );
	assertArrayClose( extractCol( Q, 0, n, LDA ), tc.Q_col1, 'Q_col1' );
	assertArrayClose( extractCol( Q, 1, n, LDA ), tc.Q_col2, 'Q_col2' );
	assertArrayClose( extractCol( Q, 2, n, LDA ), tc.Q_col3, 'Q_col3' );
	assertArrayClose( extractCol( Z, 0, n, LDA ), tc.Z_col1, 'Z_col1' );
	assertArrayClose( extractCol( Z, 1, n, LDA ), tc.Z_col2, 'Z_col2' );
	assertArrayClose( extractCol( Z, 2, n, LDA ), tc.Z_col3, 'Z_col3' );
});
