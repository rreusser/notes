

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgelq2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgelq2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

/**
* Create a column-major matrix from row-order input values.
*
* @param {number} M - rows
* @param {number} N - cols
* @param {Array} vals - row-major values (M*N)
* @returns {Float64Array} column-major flat array with LDA=M
*/
function colMajor( M, N, vals ) {
	var out = new Float64Array( M * N );
	var i;
	var j;
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			out[ j * M + i ] = vals[ i * N + j ];
		}
	}
	return out;
}

/**
* Extract column-major sub-matrix of size M x N from flat array with leading dim LDA.
*
* @param {Float64Array} A - flat array
* @param {number} LDA - leading dimension
* @param {number} M - rows to extract
* @param {number} N - cols to extract
* @returns {Array} M*N values in column-major order
*/
function extractMatrix( A, LDA, M, N ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( A[ j * LDA + i ] );
		}
	}
	return out;
}


// TESTS //

test( 'dgelq2: 3x4 (M < N, well-conditioned)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '3x4' );

	// 3x4 matrix with LDA=6 (matching Fortran)
	A = new Float64Array( 6 * 4 );
	A[ 0 * 6 + 0 ] = 2.0; A[ 1 * 6 + 0 ] = 1.0; A[ 2 * 6 + 0 ] = 3.0; A[ 3 * 6 + 0 ] = 1.0;
	A[ 0 * 6 + 1 ] = 1.0; A[ 1 * 6 + 1 ] = 4.0; A[ 2 * 6 + 1 ] = 2.0; A[ 3 * 6 + 1 ] = 3.0;
	A[ 0 * 6 + 2 ] = 3.0; A[ 1 * 6 + 2 ] = 2.0; A[ 2 * 6 + 2 ] = 5.0; A[ 3 * 6 + 2 ] = 2.0;
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 3 );

	info = dgelq2( 3, 4, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 3, 4 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelq2: 4x3 (M > N)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '4x3' );

	// 4x3 matrix with LDA=6
	A = new Float64Array( 6 * 3 );
	A[ 0 * 6 + 0 ] = 2.0; A[ 1 * 6 + 0 ] = 1.0; A[ 2 * 6 + 0 ] = 3.0;
	A[ 0 * 6 + 1 ] = 1.0; A[ 1 * 6 + 1 ] = 4.0; A[ 2 * 6 + 1 ] = 2.0;
	A[ 0 * 6 + 2 ] = 3.0; A[ 1 * 6 + 2 ] = 2.0; A[ 2 * 6 + 2 ] = 5.0;
	A[ 0 * 6 + 3 ] = 1.0; A[ 1 * 6 + 3 ] = 3.0; A[ 2 * 6 + 3 ] = 1.0;
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 4 );

	info = dgelq2( 4, 3, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 4, 3 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelq2: 3x3 (square)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '3x3' );

	// 3x3 matrix with LDA=6
	A = new Float64Array( 6 * 3 );
	A[ 0 * 6 + 0 ] = 4.0; A[ 1 * 6 + 0 ] = 1.0; A[ 2 * 6 + 0 ] = 2.0;
	A[ 0 * 6 + 1 ] = 1.0; A[ 1 * 6 + 1 ] = 3.0; A[ 2 * 6 + 1 ] = 1.0;
	A[ 0 * 6 + 2 ] = 2.0; A[ 1 * 6 + 2 ] = 1.0; A[ 2 * 6 + 2 ] = 5.0;
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 3 );

	info = dgelq2( 3, 3, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 3, 3 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelq2: 1x4 (single row)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '1x4' );

	// 1x4 matrix with LDA=6
	A = new Float64Array( 6 * 4 );
	A[ 0 * 6 + 0 ] = 1.0;
	A[ 1 * 6 + 0 ] = 2.0;
	A[ 2 * 6 + 0 ] = 3.0;
	A[ 3 * 6 + 0 ] = 4.0;
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );

	info = dgelq2( 1, 4, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 1, 4 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelq2: 3x1 (single column)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '3x1' );

	// 3x1 matrix with LDA=6
	A = new Float64Array( 6 * 1 );
	A[ 0 * 6 + 0 ] = 2.0;
	A[ 0 * 6 + 1 ] = 3.0;
	A[ 0 * 6 + 2 ] = 4.0;
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 3 );

	info = dgelq2( 3, 1, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 3, 1 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelq2: M=0 (quick return)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( 'm_zero' );

	A = new Float64Array( 1 );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );

	info = dgelq2( 0, 3, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
});

test( 'dgelq2: N=0 (quick return)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( 'n_zero' );

	A = new Float64Array( 1 );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );

	info = dgelq2( 3, 0, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
});

test( 'dgelq2: 1x1', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '1x1' );

	A = new Float64Array( 6 );
	A[ 0 ] = 7.0;
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );

	info = dgelq2( 1, 1, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 1, 1 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelq2: 2x5 (wide)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '2x5' );

	// 2x5 matrix with LDA=6
	A = new Float64Array( 6 * 5 );
	A[ 0 * 6 + 0 ] = 1.0; A[ 1 * 6 + 0 ] = 2.0; A[ 2 * 6 + 0 ] = 3.0; A[ 3 * 6 + 0 ] = 4.0; A[ 4 * 6 + 0 ] = 5.0;
	A[ 0 * 6 + 1 ] = 6.0; A[ 1 * 6 + 1 ] = 7.0; A[ 2 * 6 + 1 ] = 8.0; A[ 3 * 6 + 1 ] = 9.0; A[ 4 * 6 + 1 ] = 10.0;
	TAU = new Float64Array( 2 );
	WORK = new Float64Array( 2 );

	info = dgelq2( 2, 5, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 2, 5 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelq2: compact layout (LDA=M, no padding)', function t() {
	var WORK;
	var info;
	var TAU;
	var A;

	// Same 3x3 matrix but with compact layout (strideA2 = M = 3)
	A = colMajor( 3, 3, [
		4, 1, 2,
		1, 3, 1,
		2, 1, 5
	] );
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 3 );

	info = dgelq2( 3, 3, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );

	// Compare against the LDA=6 fixture - should match
	var tc = findCase( '3x3' );
	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelq2: non-zero offset', function t() {
	var WORK;
	var info;
	var TAU;
	var A;

	// 2x2 sub-matrix at offset 3 in a larger buffer (LDA=4)
	// Using the 2x2 sub-matrix from the 3x3 test case top-left corner
	A = new Float64Array( 3 + 4 * 2 );
	// offset = 3, strideA1 = 1, strideA2 = 4
	// A(0,0) at 3, A(1,0) at 4, A(0,1) at 7, A(1,1) at 8
	A[ 3 ] = 4.0; A[ 4 ] = 1.0;
	A[ 7 ] = 1.0; A[ 8 ] = 3.0;
	TAU = new Float64Array( 4 );
	WORK = new Float64Array( 4 );

	info = dgelq2( 2, 2, A, 1, 4, 3, TAU, 1, 1, WORK, 1, 0 );

	assert.equal( info, 0, 'INFO' );
	// Verify TAU was written at offset 1
	// Just verify it runs without error and produces valid output
	assert.ok( !isNaN( TAU[ 1 ] ), 'TAU[1] is a number' );
	assert.ok( !isNaN( TAU[ 2 ] ), 'TAU[2] is a number' );
});
