'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgelq2 = require( '../../dgelq2/lib/base.js' );
var dgelqf = require( '../../dgelqf/lib/base.js' );
var dorglq = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dorglq.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Verifies Q * Q^T = I for an M-by-N orthogonal matrix Q (real, column-major).
*
* @private
* @param {Float64Array} A - the matrix Q in column-major order
* @param {integer} M - number of rows
* @param {integer} N - number of columns
* @param {number} tol - tolerance for comparison
*/
function assertOrthogonal( A, M, N, tol ) {
	var sum;
	var i;
	var j;
	var k;

	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < M; j++ ) {
			sum = 0.0;
			for ( k = 0; k < N; k++ ) {
				// A is column-major: A(i,k) = A[k*M + i]
				sum += A[ k * M + i ] * A[ k * M + j ];
			}
			if ( i === j ) {
				assertClose( sum, 1.0, tol, 'QQT[' + i + ',' + j + ']' );
			} else {
				assert.ok( Math.abs( sum ) < tol, 'QQT[' + i + ',' + j + '] should be ~0, got ' + sum );
			}
		}
	}
}


// TESTS //

test( 'dorglq: 3x4_k3 (M < N, full K=M from LQ)', function t() {
	var tc = findCase( '3x4_k3' );
	var M = 3;
	var N = 4;
	var K = 3;
	var A = new Float64Array([
		2.0, 1.0, 3.0,
		1.0, 4.0, 2.0,
		3.0, 2.0, 5.0,
		1.0, 3.0, 2.0
	]);
	var TAU = new Float64Array( K );
	var WORK = new Float64Array( M * 32 );
	var info;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorglq(M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
	assertOrthogonal( A, M, N, 1e-14 );
});

test( 'dorglq: 3x3_k3 (square, full K=M from LQ)', function t() {
	var tc = findCase( '3x3_k3' );
	var M = 3;
	var N = 3;
	var K = 3;
	var A = new Float64Array([
		4.0, 1.0, 2.0,
		1.0, 3.0, 1.0,
		2.0, 1.0, 5.0
	]);
	var TAU = new Float64Array( K );
	var WORK = new Float64Array( M * 32 );
	var info;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorglq(M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
	assertOrthogonal( A, M, N, 1e-14 );
});

test( 'dorglq: 2x5_k2 (rectangular, M < N)', function t() {
	var tc = findCase( '2x5_k2' );
	var M = 2;
	var N = 5;
	var K = 2;
	var A = new Float64Array([
		1.0, 6.0,
		2.0, 7.0,
		3.0, 8.0,
		4.0, 9.0,
		5.0, 10.0
	]);
	var TAU = new Float64Array( K );
	var WORK = new Float64Array( M * 32 );
	var info;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorglq(M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
	assertOrthogonal( A, M, N, 1e-14 );
});

test( 'dorglq: k0_identity (K=0 produces identity)', function t() {
	var tc = findCase( 'k0_identity' );
	var M = 3;
	var N = 3;
	var K = 0;
	var A = new Float64Array([
		9.0, 9.0, 9.0,
		9.0, 9.0, 9.0,
		9.0, 9.0, 9.0
	]);
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( M * 32 );
	var info;

	info = dorglq(M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
});

test( 'dorglq: m0_quick (M=0 quick return)', function t() {
	var tc = findCase( 'm0_quick' );
	var A = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dorglq(0, 4, 0, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
});

test( 'dorglq: 1x1_k1', function t() {
	var tc = findCase( '1x1_k1' );
	var M = 1;
	var N = 1;
	var K = 1;
	var A = new Float64Array([ 7.0 ]);
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( M * 32 );
	var info;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorglq(M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
});

test( 'dorglq: 3x4_k2 (K < M, partial generation)', function t() {
	var tc = findCase( '3x4_k2' );
	var M = 3;
	var N = 4;
	var K = 2;
	var A = new Float64Array([
		2.0, 1.0, 3.0,
		1.0, 4.0, 2.0,
		3.0, 2.0, 5.0,
		1.0, 3.0, 2.0
	]);
	var TAU = new Float64Array( M );
	var WORK = new Float64Array( M * 32 );
	var info;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorglq(M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
	assertOrthogonal( A, M, N, 1e-14 );
});

test( 'dorglq: 35x40_k35_blocked (exercises blocked path, NB=32)', function t() {
	var M = 35;
	var N = 40;
	var K = 35;
	var LDA = 35;
	var A = new Float64Array( LDA * N );
	var TAU = new Float64Array( K );
	var WORK = new Float64Array( M * 32 );
	var info;
	var i;
	var j;

	// Generate a deterministic matrix
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			A[ j * LDA + i ] = ( i + 1 + j + 1 ) / ( M + N ) + 0.1 * ( ( ( i + 1 ) * ( j + 1 ) ) % 7 );
		}
	}

	// LQ factorize, then generate Q
	// Note: dgelq2 used instead of dgelqf for consistency (both produce
	// identical factorizations for unblocked panel, and dgelqf internally
	// delegates to dgelq2 per panel). Using dgelq2 avoids potential
	// blocking-order differences with the Fortran reference.
	dgelq2( M, N, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorglq(M, N, K, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, 0 );
	// Verify orthogonality: Q * Q^T = I
	assertOrthogonal( A, M, N, 1e-10 );
});

test( 'dorglq: 1x4_k1 (single row)', function t() {
	var tc = findCase( '1x4_k1' );
	var M = 1;
	var N = 4;
	var K = 1;
	var A = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( M * 32 );
	var info;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorglq(M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
});

test( 'dorglq: n0_quick (N=0 quick return)', function t() {
	var tc = findCase( 'n0_quick' );
	var A = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dorglq(0, 0, 0, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
});

test( 'dorglq: verifies Q*Q^T = I for 3x4_k3', function t() {
	var M = 3;
	var N = 4;
	var K = 3;
	var A = new Float64Array([
		2.0, 1.0, 3.0,
		1.0, 4.0, 2.0,
		3.0, 2.0, 5.0,
		1.0, 3.0, 2.0
	]);
	var TAU = new Float64Array( K );
	var WORK = new Float64Array( M * 32 );

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	dorglq(M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assertOrthogonal( A, M, N, 1e-14 );
});

test( 'dorglq: blocked K=35, M=40 (partial-block zero init)', function t() {
	// K=35 with NB=32 gives kk=min(35,32+32)=35, M=40 > kk=35,
	// so the zero-init loop for rows kk..M-1 in columns 0..kk-1 executes.
	var M = 40;
	var N = 40;
	var K = 35;
	var LDA = M;
	var A_src = new Float64Array( K * N );
	var A = new Float64Array( LDA * N );
	var TAU = new Float64Array( K );
	var WORK = new Float64Array( M * 64 );
	var seed = 88888;
	var x = seed;
	var info;
	var i;
	var j;

	// Generate a deterministic K-by-N matrix for LQ
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < K; i++ ) {
			x = ( ( x * 1103515245 ) + 12345 ) & 0x7fffffff;
			A_src[ j * K + i ] = ( ( x % 2000 ) - 1000 ) / 500.0;
		}
	}

	// LQ factorize
	dgelq2( K, N, A_src, 1, K, 0, TAU, 1, 0, WORK, 1, 0 );

	// Copy into the larger M x N matrix (first K rows from LQ, rest zero)
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < K; i++ ) {
			A[ j * LDA + i ] = A_src[ j * K + i ];
		}
	}

	info = dorglq(M, N, K, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	// Verify Q * Q^T = I_40
	assertOrthogonal( A, M, N, 1e-10 );
});
