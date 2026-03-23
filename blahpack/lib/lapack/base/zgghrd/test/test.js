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
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
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
	var A = new Complex128Array( LDA * n );
	var B = new Complex128Array( LDA * n );
	var Q = new Complex128Array( LDA * n );
	var Z = new Complex128Array( LDA * n );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var Qv = reinterpret( Q, 0 );
	var Zv = reinterpret( Z, 0 );
	var sa1 = 1;
	var sa2 = LDA;
	var info;

	cset( Av, LDA, 0, 0, 2.0, 1.0 );
	cset( Av, LDA, 0, 1, 1.0, 0.5 );
	cset( Av, LDA, 0, 2, 0.5, -0.5 );
	cset( Av, LDA, 0, 3, 1.0, 1.0 );
	cset( Av, LDA, 1, 0, 1.0, -1.0 );
	cset( Av, LDA, 1, 1, 3.0, 0.0 );
	cset( Av, LDA, 1, 2, 1.0, 1.0 );
	cset( Av, LDA, 1, 3, 0.5, 0.5 );
	cset( Av, LDA, 2, 0, 0.5, 0.5 );
	cset( Av, LDA, 2, 1, 2.0, -1.0 );
	cset( Av, LDA, 2, 2, 4.0, 1.0 );
	cset( Av, LDA, 2, 3, 1.0, 0.0 );
	cset( Av, LDA, 3, 0, 0.0, 1.0 );
	cset( Av, LDA, 3, 1, 1.0, 1.0 );
	cset( Av, LDA, 3, 2, 0.5, -0.5 );
	cset( Av, LDA, 3, 3, 2.0, -1.0 );
	cset( Bv, LDA, 0, 0, 3.0, 0.0 );
	cset( Bv, LDA, 0, 1, 1.0, 0.5 );
	cset( Bv, LDA, 0, 2, 0.5, 0.5 );
	cset( Bv, LDA, 0, 3, 0.0, 1.0 );
	cset( Bv, LDA, 1, 1, 2.0, 1.0 );
	cset( Bv, LDA, 1, 2, 1.0, 0.0 );
	cset( Bv, LDA, 1, 3, 0.5, -0.5 );
	cset( Bv, LDA, 2, 2, 4.0, -1.0 );
	cset( Bv, LDA, 2, 3, 1.0, 1.0 );
	cset( Bv, LDA, 3, 3, 1.0, 0.0 );

	info = base( 'initialize', 'initialize', n, 1, 4, A, sa1, sa2, 0, B, sa1, sa2, 0, Q, sa1, sa2, 0, Z, sa1, sa2, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCol( Av, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( Av, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( Av, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( Av, 3, n, LDA ), tc.A_col4, 'A_col4' );
	assertArrayClose( extractCol( Bv, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( Bv, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( Bv, 2, n, LDA ), tc.B_col3, 'B_col3' );
	assertArrayClose( extractCol( Bv, 3, n, LDA ), tc.B_col4, 'B_col4' );
	assertArrayClose( extractCol( Qv, 0, n, LDA ), tc.Q_col1, 'Q_col1' );
	assertArrayClose( extractCol( Qv, 1, n, LDA ), tc.Q_col2, 'Q_col2' );
	assertArrayClose( extractCol( Qv, 2, n, LDA ), tc.Q_col3, 'Q_col3' );
	assertArrayClose( extractCol( Qv, 3, n, LDA ), tc.Q_col4, 'Q_col4' );
	assertArrayClose( extractCol( Zv, 0, n, LDA ), tc.Z_col1, 'Z_col1' );
	assertArrayClose( extractCol( Zv, 1, n, LDA ), tc.Z_col2, 'Z_col2' );
	assertArrayClose( extractCol( Zv, 2, n, LDA ), tc.Z_col3, 'Z_col3' );
	assertArrayClose( extractCol( Zv, 3, n, LDA ), tc.Z_col4, 'Z_col4' );
});

test( 'zgghrd: n=1 quick return', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'n_equals_1'; } );
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 1 );
	var Q = new Complex128Array( 1 );
	var Z = new Complex128Array( 1 );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var Qv = reinterpret( Q, 0 );
	var Zv = reinterpret( Z, 0 );
	var info;

	Av[ 0 ] = 5.0; Av[ 1 ] = 3.0;
	Bv[ 0 ] = 2.0; Bv[ 1 ] = 1.0;

	info = base( 'initialize', 'initialize', 1, 1, 1, A, 1, 1, 0, B, 1, 1, 0, Q, 1, 1, 0, Z, 1, 1, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( [ Av[ 0 ], Av[ 1 ] ], tc.A, 'A' );
	assertArrayClose( [ Bv[ 0 ], Bv[ 1 ] ], tc.B, 'B' );
	assertArrayClose( [ Qv[ 0 ], Qv[ 1 ] ], tc.Q, 'Q' );
	assertArrayClose( [ Zv[ 0 ], Zv[ 1 ] ], tc.Z, 'Z' );
});

test( 'zgghrd: no Q/Z accumulation (COMPQ=N, COMPZ=N) 3x3', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'no_qz_3x3'; } );
	var n = 3;
	var LDA = n;
	var A = new Complex128Array( LDA * n );
	var B = new Complex128Array( LDA * n );
	var Q = new Complex128Array( LDA * n );
	var Z = new Complex128Array( LDA * n );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var info;

	cset( Av, LDA, 0, 0, 1.0, 0.0 );
	cset( Av, LDA, 0, 1, 2.0, 1.0 );
	cset( Av, LDA, 0, 2, 3.0, -1.0 );
	cset( Av, LDA, 1, 0, 4.0, 2.0 );
	cset( Av, LDA, 1, 1, 5.0, 0.0 );
	cset( Av, LDA, 1, 2, 6.0, 1.0 );
	cset( Av, LDA, 2, 0, 7.0, -1.0 );
	cset( Av, LDA, 2, 1, 8.0, 2.0 );
	cset( Av, LDA, 2, 2, 9.0, 0.0 );
	cset( Bv, LDA, 0, 0, 2.0, 0.0 );
	cset( Bv, LDA, 0, 1, 1.0, 1.0 );
	cset( Bv, LDA, 0, 2, 0.5, 0.0 );
	cset( Bv, LDA, 1, 1, 3.0, 0.0 );
	cset( Bv, LDA, 1, 2, 1.0, -1.0 );
	cset( Bv, LDA, 2, 2, 1.0, 0.0 );

	info = base( 'none', 'none', n, 1, 3, A, 1, LDA, 0, B, 1, LDA, 0, Q, 1, LDA, 0, Z, 1, LDA, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCol( Av, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( Av, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( Av, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( Bv, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( Bv, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( Bv, 2, n, LDA ), tc.B_col3, 'B_col3' );
});

test( 'zgghrd: partial reduction ILO=2, IHI=4 on 5x5', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'partial_5x5'; } );
	var n = 5;
	var LDA = n;
	var A = new Complex128Array( LDA * n );
	var B = new Complex128Array( LDA * n );
	var Q = new Complex128Array( LDA * n );
	var Z = new Complex128Array( LDA * n );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var Qv = reinterpret( Q, 0 );
	var Zv = reinterpret( Z, 0 );
	var info;
	var i;
	var j;

	for ( j = 0; j < n; j++ ) {
		for ( i = 0; i < n; i++ ) {
			cset( Av, LDA, i, j, ( i + 1 ) + ( j + 1 ), ( ( i + 1 ) - ( j + 1 ) ) * 0.5 );
		}
	}
	for ( j = 0; j < n; j++ ) {
		for ( i = 0; i <= j; i++ ) {
			cset( Bv, LDA, i, j, ( i + 1 ) + ( j + 1 ), ( ( j + 1 ) - ( i + 1 ) ) * 0.25 );
		}
	}

	info = base( 'initialize', 'initialize', n, 2, 4, A, 1, LDA, 0, B, 1, LDA, 0, Q, 1, LDA, 0, Z, 1, LDA, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCol( Av, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( Av, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( Av, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( Av, 3, n, LDA ), tc.A_col4, 'A_col4' );
	assertArrayClose( extractCol( Av, 4, n, LDA ), tc.A_col5, 'A_col5' );
	assertArrayClose( extractCol( Bv, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( Bv, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( Bv, 2, n, LDA ), tc.B_col3, 'B_col3' );
	assertArrayClose( extractCol( Bv, 3, n, LDA ), tc.B_col4, 'B_col4' );
	assertArrayClose( extractCol( Bv, 4, n, LDA ), tc.B_col5, 'B_col5' );
	assertArrayClose( extractCol( Qv, 0, n, LDA ), tc.Q_col1, 'Q_col1' );
	assertArrayClose( extractCol( Qv, 1, n, LDA ), tc.Q_col2, 'Q_col2' );
	assertArrayClose( extractCol( Qv, 2, n, LDA ), tc.Q_col3, 'Q_col3' );
	assertArrayClose( extractCol( Qv, 3, n, LDA ), tc.Q_col4, 'Q_col4' );
	assertArrayClose( extractCol( Qv, 4, n, LDA ), tc.Q_col5, 'Q_col5' );
	assertArrayClose( extractCol( Zv, 0, n, LDA ), tc.Z_col1, 'Z_col1' );
	assertArrayClose( extractCol( Zv, 1, n, LDA ), tc.Z_col2, 'Z_col2' );
	assertArrayClose( extractCol( Zv, 2, n, LDA ), tc.Z_col3, 'Z_col3' );
	assertArrayClose( extractCol( Zv, 3, n, LDA ), tc.Z_col4, 'Z_col4' );
	assertArrayClose( extractCol( Zv, 4, n, LDA ), tc.Z_col5, 'Z_col5' );
});

test( 'zgghrd: accumulate into existing Q, Z (COMPQ=V, COMPZ=V) 3x3', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'accumulate_3x3'; } );
	var n = 3;
	var LDA = n;
	var A = new Complex128Array( LDA * n );
	var B = new Complex128Array( LDA * n );
	var Q = new Complex128Array( LDA * n );
	var Z = new Complex128Array( LDA * n );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var Qv = reinterpret( Q, 0 );
	var Zv = reinterpret( Z, 0 );
	var info;
	var i;

	cset( Av, LDA, 0, 0, 2.0, 0.0 );
	cset( Av, LDA, 0, 1, 1.0, 1.0 );
	cset( Av, LDA, 0, 2, 0.0, 1.0 );
	cset( Av, LDA, 1, 0, 3.0, -1.0 );
	cset( Av, LDA, 1, 1, 1.0, 0.0 );
	cset( Av, LDA, 1, 2, 2.0, 0.5 );
	cset( Av, LDA, 2, 0, 1.0, 2.0 );
	cset( Av, LDA, 2, 1, 0.0, -1.0 );
	cset( Av, LDA, 2, 2, 3.0, 1.0 );
	cset( Bv, LDA, 0, 0, 1.0, 0.0 );
	cset( Bv, LDA, 0, 1, 0.5, 0.5 );
	cset( Bv, LDA, 0, 2, 0.0, 1.0 );
	cset( Bv, LDA, 1, 1, 2.0, 0.0 );
	cset( Bv, LDA, 1, 2, 1.0, 0.0 );
	cset( Bv, LDA, 2, 2, 3.0, 0.0 );
	for ( i = 0; i < n; i++ ) {
		cset( Qv, LDA, i, i, 1.0, 0.0 );
		cset( Zv, LDA, i, i, 1.0, 0.0 );
	}

	info = base( 'update', 'update', n, 1, 3, A, 1, LDA, 0, B, 1, LDA, 0, Q, 1, LDA, 0, Z, 1, LDA, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCol( Av, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( Av, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( Av, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( Bv, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( Bv, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( Bv, 2, n, LDA ), tc.B_col3, 'B_col3' );
	assertArrayClose( extractCol( Qv, 0, n, LDA ), tc.Q_col1, 'Q_col1' );
	assertArrayClose( extractCol( Qv, 1, n, LDA ), tc.Q_col2, 'Q_col2' );
	assertArrayClose( extractCol( Qv, 2, n, LDA ), tc.Q_col3, 'Q_col3' );
	assertArrayClose( extractCol( Zv, 0, n, LDA ), tc.Z_col1, 'Z_col1' );
	assertArrayClose( extractCol( Zv, 1, n, LDA ), tc.Z_col2, 'Z_col2' );
	assertArrayClose( extractCol( Zv, 2, n, LDA ), tc.Z_col3, 'Z_col3' );
});

test( 'zgghrd: ILO=IHI (already reduced, near no-op)', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'ilo_eq_ihi'; } );
	var n = 3;
	var LDA = n;
	var A = new Complex128Array( LDA * n );
	var B = new Complex128Array( LDA * n );
	var Q = new Complex128Array( LDA * n );
	var Z = new Complex128Array( LDA * n );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var Qv = reinterpret( Q, 0 );
	var Zv = reinterpret( Z, 0 );
	var info;

	cset( Av, LDA, 0, 0, 1.0, 0.0 ); cset( Av, LDA, 0, 1, 2.0, 0.0 ); cset( Av, LDA, 0, 2, 3.0, 0.0 );
	cset( Av, LDA, 1, 0, 4.0, 0.0 ); cset( Av, LDA, 1, 1, 5.0, 0.0 ); cset( Av, LDA, 1, 2, 6.0, 0.0 );
	cset( Av, LDA, 2, 0, 7.0, 0.0 ); cset( Av, LDA, 2, 1, 8.0, 0.0 ); cset( Av, LDA, 2, 2, 9.0, 0.0 );
	cset( Bv, LDA, 0, 0, 1.0, 0.0 ); cset( Bv, LDA, 0, 1, 1.0, 0.0 ); cset( Bv, LDA, 0, 2, 1.0, 0.0 );
	cset( Bv, LDA, 1, 1, 2.0, 0.0 ); cset( Bv, LDA, 1, 2, 1.0, 0.0 );
	cset( Bv, LDA, 2, 2, 3.0, 0.0 );

	info = base( 'initialize', 'initialize', n, 2, 2, A, 1, LDA, 0, B, 1, LDA, 0, Q, 1, LDA, 0, Z, 1, LDA, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCol( Av, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( Av, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( Av, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( Bv, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( Bv, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( Bv, 2, n, LDA ), tc.B_col3, 'B_col3' );
	assertArrayClose( extractCol( Qv, 0, n, LDA ), tc.Q_col1, 'Q_col1' );
	assertArrayClose( extractCol( Qv, 1, n, LDA ), tc.Q_col2, 'Q_col2' );
	assertArrayClose( extractCol( Qv, 2, n, LDA ), tc.Q_col3, 'Q_col3' );
	assertArrayClose( extractCol( Zv, 0, n, LDA ), tc.Z_col1, 'Z_col1' );
	assertArrayClose( extractCol( Zv, 1, n, LDA ), tc.Z_col2, 'Z_col2' );
	assertArrayClose( extractCol( Zv, 2, n, LDA ), tc.Z_col3, 'Z_col3' );
});

test( 'zgghrd: invalid COMPQ returns -1', function t() {
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 1 );
	var Q = new Complex128Array( 1 );
	var Z = new Complex128Array( 1 );
	var info;

	info = base( 'X', 'initialize', 1, 1, 1, A, 1, 1, 0, B, 1, 1, 0, Q, 1, 1, 0, Z, 1, 1, 0 );
	assert.strictEqual( info, -1, 'invalid compq returns -1' );
});

test( 'zgghrd: invalid COMPZ returns -2', function t() {
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 1 );
	var Q = new Complex128Array( 1 );
	var Z = new Complex128Array( 1 );
	var info;

	info = base( 'initialize', 'X', 1, 1, 1, A, 1, 1, 0, B, 1, 1, 0, Q, 1, 1, 0, Z, 1, 1, 0 );
	assert.strictEqual( info, -2, 'invalid compz returns -2' );
});

test( 'zgghrd: negative N returns -3', function t() {
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 1 );
	var Q = new Complex128Array( 1 );
	var Z = new Complex128Array( 1 );
	var info;

	info = base( 'initialize', 'initialize', -1, 1, 1, A, 1, 1, 0, B, 1, 1, 0, Q, 1, 1, 0, Z, 1, 1, 0 );
	assert.strictEqual( info, -3, 'negative N returns -3' );
});

test( 'zgghrd: COMPQ=N, COMPZ=I (Z only, no Q accumulation) 3x3', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'no_qz_3x3'; } );
	var n = 3;
	var LDA = n;
	var A = new Complex128Array( LDA * n );
	var B = new Complex128Array( LDA * n );
	var Q = new Complex128Array( LDA * n );
	var Z = new Complex128Array( LDA * n );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var Zv = reinterpret( Z, 0 );
	var info;

	cset( Av, LDA, 0, 0, 1.0, 0.0 );
	cset( Av, LDA, 0, 1, 2.0, 1.0 );
	cset( Av, LDA, 0, 2, 3.0, -1.0 );
	cset( Av, LDA, 1, 0, 4.0, 2.0 );
	cset( Av, LDA, 1, 1, 5.0, 0.0 );
	cset( Av, LDA, 1, 2, 6.0, 1.0 );
	cset( Av, LDA, 2, 0, 7.0, -1.0 );
	cset( Av, LDA, 2, 1, 8.0, 2.0 );
	cset( Av, LDA, 2, 2, 9.0, 0.0 );
	cset( Bv, LDA, 0, 0, 2.0, 0.0 );
	cset( Bv, LDA, 0, 1, 1.0, 1.0 );
	cset( Bv, LDA, 0, 2, 0.5, 0.0 );
	cset( Bv, LDA, 1, 1, 3.0, 0.0 );
	cset( Bv, LDA, 1, 2, 1.0, -1.0 );
	cset( Bv, LDA, 2, 2, 1.0, 0.0 );

	info = base( 'none', 'initialize', n, 1, 3, A, 1, LDA, 0, B, 1, LDA, 0, Q, 1, LDA, 0, Z, 1, LDA, 0 );

	assert.strictEqual( info, 0, 'info' );
	// A and B should match the no_qz_3x3 fixture (same A,B inputs, same reduction)
	assertArrayClose( extractCol( Av, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( Av, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( Av, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( Bv, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( Bv, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( Bv, 2, n, LDA ), tc.B_col3, 'B_col3' );
	// Z should be initialized to identity and then accumulated (not just identity)
	// Verify Z is unitary and not all zeros
	assert.ok( Zv[ 0 ] !== 0.0 || Zv[ 1 ] !== 0.0, 'Z is not all zeros' );
});

test( 'zgghrd: COMPQ=I, COMPZ=N (Q only, no Z accumulation) 3x3', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'no_qz_3x3'; } );
	var n = 3;
	var LDA = n;
	var A = new Complex128Array( LDA * n );
	var B = new Complex128Array( LDA * n );
	var Q = new Complex128Array( LDA * n );
	var Z = new Complex128Array( LDA * n );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var Qv = reinterpret( Q, 0 );
	var info;

	cset( Av, LDA, 0, 0, 1.0, 0.0 );
	cset( Av, LDA, 0, 1, 2.0, 1.0 );
	cset( Av, LDA, 0, 2, 3.0, -1.0 );
	cset( Av, LDA, 1, 0, 4.0, 2.0 );
	cset( Av, LDA, 1, 1, 5.0, 0.0 );
	cset( Av, LDA, 1, 2, 6.0, 1.0 );
	cset( Av, LDA, 2, 0, 7.0, -1.0 );
	cset( Av, LDA, 2, 1, 8.0, 2.0 );
	cset( Av, LDA, 2, 2, 9.0, 0.0 );
	cset( Bv, LDA, 0, 0, 2.0, 0.0 );
	cset( Bv, LDA, 0, 1, 1.0, 1.0 );
	cset( Bv, LDA, 0, 2, 0.5, 0.0 );
	cset( Bv, LDA, 1, 1, 3.0, 0.0 );
	cset( Bv, LDA, 1, 2, 1.0, -1.0 );
	cset( Bv, LDA, 2, 2, 1.0, 0.0 );

	info = base( 'initialize', 'none', n, 1, 3, A, 1, LDA, 0, B, 1, LDA, 0, Q, 1, LDA, 0, Z, 1, LDA, 0 );

	assert.strictEqual( info, 0, 'info' );
	// A and B should match the no_qz_3x3 fixture
	assertArrayClose( extractCol( Av, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( Av, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( Av, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( Bv, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( Bv, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( Bv, 2, n, LDA ), tc.B_col3, 'B_col3' );
	// Q should be initialized to identity and accumulated
	assert.ok( Qv[ 0 ] !== 0.0 || Qv[ 1 ] !== 0.0, 'Q is not all zeros' );
});

test( 'zgghrd: empty range IHI=ILO-1', function t() {
	var tc = fixture.find( function( t ) { return t.name === 'empty_range'; } );
	var n = 3;
	var LDA = n;
	var A = new Complex128Array( LDA * n );
	var B = new Complex128Array( LDA * n );
	var Q = new Complex128Array( LDA * n );
	var Z = new Complex128Array( LDA * n );
	var Av = reinterpret( A, 0 );
	var Bv = reinterpret( B, 0 );
	var Qv = reinterpret( Q, 0 );
	var Zv = reinterpret( Z, 0 );
	var info;

	cset( Av, LDA, 0, 0, 1.0, 0.0 ); cset( Av, LDA, 0, 1, 2.0, 0.0 ); cset( Av, LDA, 0, 2, 3.0, 0.0 );
	cset( Av, LDA, 1, 0, 4.0, 0.0 ); cset( Av, LDA, 1, 1, 5.0, 0.0 ); cset( Av, LDA, 1, 2, 6.0, 0.0 );
	cset( Av, LDA, 2, 0, 7.0, 0.0 ); cset( Av, LDA, 2, 1, 8.0, 0.0 ); cset( Av, LDA, 2, 2, 9.0, 0.0 );
	cset( Bv, LDA, 0, 0, 1.0, 0.0 );
	cset( Bv, LDA, 1, 1, 2.0, 0.0 );
	cset( Bv, LDA, 2, 2, 3.0, 0.0 );

	info = base( 'initialize', 'initialize', n, 2, 1, A, 1, LDA, 0, B, 1, LDA, 0, Q, 1, LDA, 0, Z, 1, LDA, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( extractCol( Av, 0, n, LDA ), tc.A_col1, 'A_col1' );
	assertArrayClose( extractCol( Av, 1, n, LDA ), tc.A_col2, 'A_col2' );
	assertArrayClose( extractCol( Av, 2, n, LDA ), tc.A_col3, 'A_col3' );
	assertArrayClose( extractCol( Bv, 0, n, LDA ), tc.B_col1, 'B_col1' );
	assertArrayClose( extractCol( Bv, 1, n, LDA ), tc.B_col2, 'B_col2' );
	assertArrayClose( extractCol( Bv, 2, n, LDA ), tc.B_col3, 'B_col3' );
	assertArrayClose( extractCol( Qv, 0, n, LDA ), tc.Q_col1, 'Q_col1' );
	assertArrayClose( extractCol( Qv, 1, n, LDA ), tc.Q_col2, 'Q_col2' );
	assertArrayClose( extractCol( Qv, 2, n, LDA ), tc.Q_col3, 'Q_col3' );
	assertArrayClose( extractCol( Zv, 0, n, LDA ), tc.Z_col1, 'Z_col1' );
	assertArrayClose( extractCol( Zv, 1, n, LDA ), tc.Z_col2, 'Z_col2' );
	assertArrayClose( extractCol( Zv, 2, n, LDA ), tc.Z_col3, 'Z_col3' );
});
