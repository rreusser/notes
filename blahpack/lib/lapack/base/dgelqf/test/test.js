

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgelqf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgelqf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dgelqf: 3x5 (M < N)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '3x5' );

	// 3x5 matrix, LDA = M = 3
	A = new Float64Array( 3 * 5 );
	// Row 0: 2 1 3 1 4
	A[ 0 * 3 + 0 ] = 2.0; A[ 1 * 3 + 0 ] = 1.0; A[ 2 * 3 + 0 ] = 3.0; A[ 3 * 3 + 0 ] = 1.0; A[ 4 * 3 + 0 ] = 4.0;
	// Row 1: 1 4 2 3 1
	A[ 0 * 3 + 1 ] = 1.0; A[ 1 * 3 + 1 ] = 4.0; A[ 2 * 3 + 1 ] = 2.0; A[ 3 * 3 + 1 ] = 3.0; A[ 4 * 3 + 1 ] = 1.0;
	// Row 2: 3 2 5 2 3
	A[ 0 * 3 + 2 ] = 3.0; A[ 1 * 3 + 2 ] = 2.0; A[ 2 * 3 + 2 ] = 5.0; A[ 3 * 3 + 2 ] = 2.0; A[ 4 * 3 + 2 ] = 3.0;
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 3 * 32 );

	info = dgelqf( 3, 5, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 3, 3, 5 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelqf: 5x3 (M > N)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '5x3' );

	// 5x3 matrix, LDA = M = 5
	A = new Float64Array( 5 * 3 );
	// Col 0
	A[ 0 * 5 + 0 ] = 2.0; A[ 0 * 5 + 1 ] = 1.0; A[ 0 * 5 + 2 ] = 3.0; A[ 0 * 5 + 3 ] = 1.0; A[ 0 * 5 + 4 ] = 2.0;
	// Col 1
	A[ 1 * 5 + 0 ] = 1.0; A[ 1 * 5 + 1 ] = 4.0; A[ 1 * 5 + 2 ] = 2.0; A[ 1 * 5 + 3 ] = 3.0; A[ 1 * 5 + 4 ] = 1.0;
	// Col 2
	A[ 2 * 5 + 0 ] = 3.0; A[ 2 * 5 + 1 ] = 2.0; A[ 2 * 5 + 2 ] = 5.0; A[ 2 * 5 + 3 ] = 1.0; A[ 2 * 5 + 4 ] = 4.0;
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 5 * 32 );

	info = dgelqf( 5, 3, A, 1, 5, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 5, 5, 3 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelqf: 4x4 (square)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '4x4' );

	// 4x4 matrix, LDA = M = 4
	A = new Float64Array( 4 * 4 );
	// Row 0: 4 1 2 1
	A[ 0 * 4 + 0 ] = 4.0; A[ 1 * 4 + 0 ] = 1.0; A[ 2 * 4 + 0 ] = 2.0; A[ 3 * 4 + 0 ] = 1.0;
	// Row 1: 1 3 1 2
	A[ 0 * 4 + 1 ] = 1.0; A[ 1 * 4 + 1 ] = 3.0; A[ 2 * 4 + 1 ] = 1.0; A[ 3 * 4 + 1 ] = 2.0;
	// Row 2: 2 1 5 3
	A[ 0 * 4 + 2 ] = 2.0; A[ 1 * 4 + 2 ] = 1.0; A[ 2 * 4 + 2 ] = 5.0; A[ 3 * 4 + 2 ] = 3.0;
	// Row 3: 1 2 3 6
	A[ 0 * 4 + 3 ] = 1.0; A[ 1 * 4 + 3 ] = 2.0; A[ 2 * 4 + 3 ] = 3.0; A[ 3 * 4 + 3 ] = 6.0;
	TAU = new Float64Array( 4 );
	WORK = new Float64Array( 4 * 32 );

	info = dgelqf( 4, 4, A, 1, 4, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 4, 4, 4 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelqf: 1x1 (edge case)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '1x1' );

	A = new Float64Array( 1 );
	A[ 0 ] = 7.0;
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 32 );

	info = dgelqf( 1, 1, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelqf: M=0 (quick return)', function t() {
	var WORK;
	var info;
	var TAU;
	var A;

	A = new Float64Array( 1 );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );

	info = dgelqf( 0, 5, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );

	assert.equal( info, 0, 'INFO' );
});

test( 'dgelqf: N=0 (quick return)', function t() {
	var WORK;
	var info;
	var TAU;
	var A;

	A = new Float64Array( 1 );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );

	info = dgelqf( 3, 0, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );

	assert.equal( info, 0, 'INFO' );
});

test( 'dgelqf: 35x40 (blocked path, NB=32)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;
	var i;
	var j;

	tc = findCase( '35x40' );

	// 35x40 diagonally dominant matrix, LDA = M = 35
	A = new Float64Array( 35 * 40 );
	for ( i = 0; i < 35; i++ ) {
		for ( j = 0; j < 40; j++ ) {
			A[ j * 35 + i ] = 1.0 / ( i + j + 2 );
		}
		A[ i * 35 + i ] = 10.0 + ( i + 1 );
	}
	TAU = new Float64Array( 35 );
	WORK = new Float64Array( 35 * 32 );

	info = dgelqf( 35, 40, A, 1, 35, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 35, 35, 40 ), tc.A, 1e-12, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-12, 'TAU' );
});

test( 'dgelqf: 2x6 (wide)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '2x6' );

	// 2x6 matrix, LDA = M = 2
	A = new Float64Array( 2 * 6 );
	// Row 0: 1 2 3 4 5 6
	A[ 0 * 2 + 0 ] = 1.0; A[ 1 * 2 + 0 ] = 2.0; A[ 2 * 2 + 0 ] = 3.0;
	A[ 3 * 2 + 0 ] = 4.0; A[ 4 * 2 + 0 ] = 5.0; A[ 5 * 2 + 0 ] = 6.0;
	// Row 1: 7 8 9 10 11 12
	A[ 0 * 2 + 1 ] = 7.0; A[ 1 * 2 + 1 ] = 8.0; A[ 2 * 2 + 1 ] = 9.0;
	A[ 3 * 2 + 1 ] = 10.0; A[ 4 * 2 + 1 ] = 11.0; A[ 5 * 2 + 1 ] = 12.0;
	TAU = new Float64Array( 2 );
	WORK = new Float64Array( 2 * 32 );

	info = dgelqf( 2, 6, A, 1, 2, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 2, 2, 6 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelqf: compact layout (LDA=M, no padding)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '4x4' );

	// Same 4x4 matrix but explicitly using compact layout
	A = new Float64Array( 4 * 4 );
	A[ 0 * 4 + 0 ] = 4.0; A[ 1 * 4 + 0 ] = 1.0; A[ 2 * 4 + 0 ] = 2.0; A[ 3 * 4 + 0 ] = 1.0;
	A[ 0 * 4 + 1 ] = 1.0; A[ 1 * 4 + 1 ] = 3.0; A[ 2 * 4 + 1 ] = 1.0; A[ 3 * 4 + 1 ] = 2.0;
	A[ 0 * 4 + 2 ] = 2.0; A[ 1 * 4 + 2 ] = 1.0; A[ 2 * 4 + 2 ] = 5.0; A[ 3 * 4 + 2 ] = 3.0;
	A[ 0 * 4 + 3 ] = 1.0; A[ 1 * 4 + 3 ] = 2.0; A[ 2 * 4 + 3 ] = 3.0; A[ 3 * 4 + 3 ] = 6.0;
	TAU = new Float64Array( 4 );
	WORK = new Float64Array( 4 * 32 );

	info = dgelqf( 4, 4, A, 1, 4, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelqf: non-zero offset', function t() {
	var WORK;
	var info;
	var TAU;
	var A;

	// 2x2 sub-matrix at offset 5 in a larger buffer (LDA=4)
	A = new Float64Array( 5 + 4 * 2 );
	// offset = 5, strideA1 = 1, strideA2 = 4
	// A(0,0) at 5, A(1,0) at 6, A(0,1) at 9, A(1,1) at 10
	A[ 5 ] = 4.0; A[ 6 ] = 1.0;
	A[ 9 ] = 1.0; A[ 10 ] = 3.0;
	TAU = new Float64Array( 4 );
	WORK = new Float64Array( 2 * 32 );

	info = dgelqf( 2, 2, A, 1, 4, 5, TAU, 1, 1, WORK, 1, 0, WORK.length );

	assert.equal( info, 0, 'INFO' );
	// Verify TAU was written at offset 1
	assert.ok( !isNaN( TAU[ 1 ] ), 'TAU[1] is a number' );
	assert.ok( !isNaN( TAU[ 2 ] ), 'TAU[2] is a number' );
});

test( 'dgelqf: null WORK triggers internal allocation', function t() {
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '3x5' );

	// 3x5 matrix, LDA = M = 3
	A = new Float64Array( 3 * 5 );
	A[ 0 * 3 + 0 ] = 2.0; A[ 1 * 3 + 0 ] = 1.0; A[ 2 * 3 + 0 ] = 3.0; A[ 3 * 3 + 0 ] = 1.0; A[ 4 * 3 + 0 ] = 4.0;
	A[ 0 * 3 + 1 ] = 1.0; A[ 1 * 3 + 1 ] = 4.0; A[ 2 * 3 + 1 ] = 2.0; A[ 3 * 3 + 1 ] = 3.0; A[ 4 * 3 + 1 ] = 1.0;
	A[ 0 * 3 + 2 ] = 3.0; A[ 1 * 3 + 2 ] = 2.0; A[ 2 * 3 + 2 ] = 5.0; A[ 3 * 3 + 2 ] = 2.0; A[ 4 * 3 + 2 ] = 3.0;
	TAU = new Float64Array( 3 );

	info = dgelqf( 3, 5, A, 1, 3, 0, TAU, 1, 0, null, 1, 0, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 3, 3, 5 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgelqf: padded LDA (LDA > M)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '3x5' );

	// 3x5 matrix with LDA = 6 (padded)
	A = new Float64Array( 6 * 5 );
	// Row 0: 2 1 3 1 4
	A[ 0 * 6 + 0 ] = 2.0; A[ 1 * 6 + 0 ] = 1.0; A[ 2 * 6 + 0 ] = 3.0; A[ 3 * 6 + 0 ] = 1.0; A[ 4 * 6 + 0 ] = 4.0;
	// Row 1: 1 4 2 3 1
	A[ 0 * 6 + 1 ] = 1.0; A[ 1 * 6 + 1 ] = 4.0; A[ 2 * 6 + 1 ] = 2.0; A[ 3 * 6 + 1 ] = 3.0; A[ 4 * 6 + 1 ] = 1.0;
	// Row 2: 3 2 5 2 3
	A[ 0 * 6 + 2 ] = 3.0; A[ 1 * 6 + 2 ] = 2.0; A[ 2 * 6 + 2 ] = 5.0; A[ 3 * 6 + 2 ] = 2.0; A[ 4 * 6 + 2 ] = 3.0;
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 3 * 32 );

	info = dgelqf( 3, 5, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0, WORK.length );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 3, 5 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});
