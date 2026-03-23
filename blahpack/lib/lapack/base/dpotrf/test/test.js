'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dpotrf = require( './../lib/base.js' );
var dpotrf2 = require( './../../dpotrf2/lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpotrf.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Creates a random N-by-N SPD matrix (col-major) by computing A = B^T * B + N*I.
*/
function randomSPD( N ) {
	var A = new Float64Array( N * N );
	var B = new Float64Array( N * N );
	var i;
	var j;
	var k;
	for ( i = 0; i < N * N; i++ ) {
		B[ i ] = ( i * 7 + 3 ) % 13 - 6; // deterministic pseudo-random
	}
	// A = B^T * B
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			var sum = 0.0;
			for ( k = 0; k < N; k++ ) {
				sum += B[ k + i * N ] * B[ k + j * N ];
			}
			A[ i + j * N ] = sum;
		}
	}
	// Add N*I for strong positive definiteness
	for ( i = 0; i < N; i++ ) {
		A[ i + i * N ] += N;
	}
	return A;
}


// TESTS //

test( 'dpotrf: lower_3x3', function t() {
	var tc = findCase( 'lower_3x3' );
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	var info = dpotrf( 'lower', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.L, 1e-14, 'L' );
});

test( 'dpotrf: upper_3x3', function t() {
	var tc = findCase( 'upper_3x3' );
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	var info = dpotrf( 'upper', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.U, 1e-14, 'U' );
});

test( 'dpotrf: lower_4x4', function t() {
	var tc = findCase( 'lower_4x4' );
	var A = new Float64Array( [ 4, 2, 1, 0, 2, 5, 3, 1, 1, 3, 9, 2, 0, 1, 2, 8 ] );
	var info = dpotrf( 'lower', 4, A, 1, 4, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.L, 1e-14, 'L' );
});

test( 'dpotrf: upper_4x4', function t() {
	var tc = findCase( 'upper_4x4' );
	var A = new Float64Array( [ 4, 2, 1, 0, 2, 5, 3, 1, 1, 3, 9, 2, 0, 1, 2, 8 ] );
	var info = dpotrf( 'upper', 4, A, 1, 4, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.U, 1e-14, 'U' );
});

test( 'dpotrf: not_posdef', function t() {
	var tc = findCase( 'not_posdef' );
	var A = new Float64Array( [ 1, 2, 3, 2, 1, 4, 3, 4, 1 ] );
	var info = dpotrf( 'lower', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'dpotrf: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var info = dpotrf( 'lower', 0, A, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dpotrf: large lower (blocked path) matches dpotrf2', function t() {
	// N=80 > NB=64, forces the blocked code path
	var N = 80;
	var A1 = randomSPD( N );
	var A2 = new Float64Array( A1 );
	var info1 = dpotrf( 'lower', N, A1, 1, N, 0 );
	var info2 = dpotrf2( 'lower', N, A2, 1, N, 0 );
	assert.equal( info1, 0 );
	assert.equal( info2, 0 );
	assertArrayClose( Array.from( A1 ), Array.from( A2 ), 1e-12, 'large lower blocked vs unblocked' );
});

test( 'dpotrf: large upper (blocked path) matches dpotrf2', function t() {
	var N = 80;
	var A1 = randomSPD( N );
	var A2 = new Float64Array( A1 );
	var info1 = dpotrf( 'upper', N, A1, 1, N, 0 );
	var info2 = dpotrf2( 'upper', N, A2, 1, N, 0 );
	assert.equal( info1, 0 );
	assert.equal( info2, 0 );
	assertArrayClose( Array.from( A1 ), Array.from( A2 ), 1e-12, 'large upper blocked vs unblocked' );
});

test( 'dpotrf: large not-posdef (blocked path)', function t() {
	// Make a matrix that fails during factorization
	var N = 80;
	var A = randomSPD( N );
	// Make the last diagonal negative
	A[ (N - 1) + (N - 1) * N ] = -1000.0;
	var info = dpotrf( 'lower', N, A, 1, N, 0 );
	assert.ok( info > 0 );
});

test( 'dpotrf: large not-posdef upper (blocked path)', function t() {
	var N = 80;
	var A = randomSPD( N );
	A[ (N - 1) + (N - 1) * N ] = -1000.0;
	var info = dpotrf( 'upper', N, A, 1, N, 0 );
	assert.ok( info > 0 );
});
