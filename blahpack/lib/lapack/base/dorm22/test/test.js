/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dorm22 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dorm22.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

// Build the 5x5 banded Q from the Fortran test, packed column-major tight.
function buildQ5() {
	var Q = new Float64Array( 25 );
	// Fortran Q(i,j) with i,j 1-based. Column-major: Q[(j-1)*5 + (i-1)].
	function set( i, j, v ) { Q[ ((j - 1) * 5) + (i - 1) ] = v; }
	set( 1, 1, 0.5 );   set( 1, 2, -0.3 );
	set( 2, 1, 0.2 );   set( 2, 2, 0.8 );
	set( 3, 1, -0.4 );  set( 3, 2, 0.1 );
	set( 1, 3, 1.1 );
	set( 2, 3, 0.7 );   set( 2, 4, 0.9 );
	set( 3, 3, -0.5 );  set( 3, 4, 0.4 );  set( 3, 5, 1.2 );
	set( 4, 1, 0.6 );   set( 4, 2, -0.2 );
	                    set( 5, 2, 1.3 );
	set( 4, 3, 0.3 );   set( 4, 4, -0.1 ); set( 4, 5, 0.5 );
	set( 5, 3, -0.7 );  set( 5, 4, 0.4 );  set( 5, 5, 0.2 );
	return Q;
}

function buildC( M, N ) {
	var C = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ (j * M) + i ] = ( ( (i + 1) - 2 + (j + 1) ) * 0.25 ) + 0.1;
		}
	}
	return C;
}


// TESTS //

test( 'dorm22: left_notrans', function t() {
	var tc = findCase( 'left_notrans' );
	var M = 5;
	var N = 4;
	var Q = buildQ5();
	var C = buildC( M, N );
	var WORK = new Float64Array( 200 );
	var info = dorm22( 'left', 'no-transpose', M, N, 3, 2, Q, 1, 5, 0, C, 1, M, 0, WORK, 1, 0, 200 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( C ), tc.c, 1e-13, 'c' );
});

test( 'dorm22: left_trans', function t() {
	var tc = findCase( 'left_trans' );
	var M = 5;
	var N = 4;
	var Q = buildQ5();
	var C = buildC( M, N );
	var WORK = new Float64Array( 200 );
	var info = dorm22( 'left', 'transpose', M, N, 3, 2, Q, 1, 5, 0, C, 1, M, 0, WORK, 1, 0, 200 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( C ), tc.c, 1e-13, 'c' );
});

test( 'dorm22: right_notrans', function t() {
	var tc = findCase( 'right_notrans' );
	var M = 4;
	var N = 5;
	var Q = buildQ5();
	var C = buildC( M, N );
	var WORK = new Float64Array( 200 );
	var info = dorm22( 'right', 'no-transpose', M, N, 3, 2, Q, 1, 5, 0, C, 1, M, 0, WORK, 1, 0, 200 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( C ), tc.c, 1e-13, 'c' );
});

test( 'dorm22: right_trans', function t() {
	var tc = findCase( 'right_trans' );
	var M = 4;
	var N = 5;
	var Q = buildQ5();
	var C = buildC( M, N );
	var WORK = new Float64Array( 200 );
	var info = dorm22( 'right', 'transpose', M, N, 3, 2, Q, 1, 5, 0, C, 1, M, 0, WORK, 1, 0, 200 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( C ), tc.c, 1e-13, 'c' );
});

test( 'dorm22: n1_zero_left_notrans', function t() {
	var tc = findCase( 'n1_zero_left_notrans' );
	var M = 3;
	var N = 4;
	// Pure 3x3 upper triangular Q.
	var Q = new Float64Array( 9 );
	Q[ 0 ] = 1.0;   // (1,1)
	Q[ 3 ] = 0.5;   // (1,2)
	Q[ 4 ] = 0.8;   // (2,2)
	Q[ 6 ] = -0.2;  // (1,3)
	Q[ 7 ] = 0.3;   // (2,3)
	Q[ 8 ] = 1.2;   // (3,3)
	var C = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ (j * M) + i ] = ( (i + 1) + (j + 1) ) * 0.3;
		}
	}
	var WORK = new Float64Array( 200 );
	var info = dorm22( 'left', 'no-transpose', M, N, 0, 3, Q, 1, M, 0, C, 1, M, 0, WORK, 1, 0, 200 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( C ), tc.c, 1e-13, 'c' );
});

test( 'dorm22: n2_zero_right_trans', function t() {
	var tc = findCase( 'n2_zero_right_trans' );
	var M = 4;
	var N = 3;
	// Pure 3x3 lower triangular Q.
	var Q = new Float64Array( 9 );
	Q[ 0 ] = 1.0;   // (1,1)
	Q[ 1 ] = 0.4;   // (2,1)
	Q[ 2 ] = -0.3;  // (3,1)
	Q[ 4 ] = 0.9;   // (2,2)
	Q[ 5 ] = 0.6;   // (3,2)
	Q[ 8 ] = 1.1;   // (3,3)
	var C = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			C[ (j * M) + i ] = ( ( (i + 1) - (j + 1) ) * 0.2 ) + 0.5;
		}
	}
	var WORK = new Float64Array( 200 );
	var info = dorm22( 'right', 'transpose', M, N, 3, 0, Q, 1, N, 0, C, 1, M, 0, WORK, 1, 0, 200 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( C ), tc.c, 1e-13, 'c' );
});

test( 'dorm22: m_zero', function t() {
	var tc = findCase( 'm_zero' );
	var Q = new Float64Array( 1 );
	var C = new Float64Array( 1 );
	var WORK = new Float64Array( 10 );
	var info = dorm22( 'left', 'no-transpose', 0, 4, 0, 0, Q, 1, 1, 0, C, 1, 1, 0, WORK, 1, 0, 10 );
	assert.equal( info, tc.info, 'info' );
});

test( 'dorm22: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var Q = new Float64Array( 25 );
	var C = new Float64Array( 5 );
	var WORK = new Float64Array( 10 );
	var info = dorm22( 'left', 'no-transpose', 5, 0, 3, 2, Q, 1, 5, 0, C, 1, 5, 0, WORK, 1, 0, 10 );
	assert.equal( info, tc.info, 'info' );
});

test( 'dorm22: small LWORK blocks execution', function t() {
	var tc = findCase( 'left_notrans' );
	var M = 5;
	var N = 4;
	var Q = buildQ5();
	var C = buildC( M, N );
	// Only enough LWORK for a single column (nb=1 blocks).
	var WORK = new Float64Array( 200 );
	var info = dorm22( 'left', 'no-transpose', M, N, 3, 2, Q, 1, 5, 0, C, 1, M, 0, WORK, 1, 0, M );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( C ), tc.c, 1e-13, 'c' );
});

test( 'dorm22: LWORK too small returns -12', function t() {
	var Q = buildQ5();
	var C = buildC( 5, 4 );
	var WORK = new Float64Array( 10 );
	var info = dorm22( 'left', 'no-transpose', 5, 4, 3, 2, Q, 1, 5, 0, C, 1, 5, 0, WORK, 1, 0, 1 );
	assert.equal( info, -12, 'info' );
});

test( 'dorm22: right side blocked with small LWORK', function t() {
	var tc = findCase( 'right_notrans' );
	var M = 4;
	var N = 5;
	var Q = buildQ5();
	var C = buildC( M, N );
	var WORK = new Float64Array( 200 );
	// Provide LWORK = N (just enough for one row block)
	var info = dorm22( 'right', 'no-transpose', M, N, 3, 2, Q, 1, 5, 0, C, 1, M, 0, WORK, 1, 0, N );
	assert.equal( info, 0, 'info' );
	assertArrayClose( Array.from( C ), tc.c, 1e-13, 'c' );
});
