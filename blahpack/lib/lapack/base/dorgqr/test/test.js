

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dgeqrf = require( '../../dgeqrf/lib/base.js' );
var dorgqr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dorgqr.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Extracts the matrix Q from a column-major array A(LDA, N) as a flat
* column-major array of shape M x N (matching Fortran print_matrix output).
*/
function extractMatrix( A, LDA, M, N ) {
	var out = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out[ j * M + i ] = A[ j * LDA + i ];
		}
	}
	return out;
}

/**
* Helper: set up a column-major matrix, run dgeqrf, then dorgqr
* Returns { info, Q } where Q is the flat M x N column-major result.
*/
function runDorgqr( inputA, M, N, K, LDA ) {
	var WORK = new Float64Array( Math.max( N * 32, 1 ) );
	var TAU = new Float64Array( Math.min( M, N ) );
	var A = new Float64Array( LDA * N );
	var info;
	var i;
	var j;

	// Copy input into column-major A(LDA, N)
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			A[ j * LDA + i ] = inputA[ j * M + i ];
		}
	}

	// Run dgeqrf to get Householder reflectors + TAU
	dgeqrf( M, N, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0 );

	// Run dorgqr to generate Q
	info = dorgqr( M, N, K, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0, N * 32 );

	return {
		info: info,
		Q: extractMatrix( A, LDA, M, N )
	};
}


// TESTS //

test( 'dorgqr: 4x3_k3', function t() {
	var tc = findCase( '4x3_k3' );
	var result = runDorgqr(
		new Float64Array([ 2, 1, 3, 1, 1, 4, 2, 3, 3, 2, 5, 1 ]),
		4, 3, 3, 4
	);
	assert.equal( result.info, tc.INFO );
	assertArrayClose( result.Q, tc.Q, 1e-14, 'Q' );
});

test( 'dorgqr: 3x3_k3', function t() {
	var tc = findCase( '3x3_k3' );
	var result = runDorgqr(
		new Float64Array([ 4, 3, 1, 1, 2, 5, 2, 1, 3 ]),
		3, 3, 3, 3
	);
	assert.equal( result.info, tc.INFO );
	assertArrayClose( result.Q, tc.Q, 1e-14, 'Q' );
});

test( 'dorgqr: 4x2_k2', function t() {
	var tc = findCase( '4x2_k2' );
	var result = runDorgqr(
		new Float64Array([ 1, 3, 5, 7, 2, 4, 6, 8 ]),
		4, 2, 2, 4
	);
	assert.equal( result.info, tc.INFO );
	assertArrayClose( result.Q, tc.Q, 1e-14, 'Q' );
});

test( 'dorgqr: k_zero (identity)', function t() {
	var tc = findCase( 'k_zero' );
	var WORK = new Float64Array( 64 );
	var TAU = new Float64Array( 2 );
	var A = new Float64Array( 6 );
	var info;

	// Fill A with arbitrary values (shouldn't matter, K=0 => identity)
	A[ 0 ] = 99; A[ 1 ] = 77; A[ 2 ] = 55;
	A[ 3 ] = 88; A[ 4 ] = 66; A[ 5 ] = 44;

	info = dorgqr( 3, 2, 0, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0, 64 );

	assert.equal( info, tc.INFO );
	assertArrayClose( extractMatrix( A, 3, 3, 2 ), tc.Q, 1e-14, 'Q' );
});

test( 'dorgqr: n_zero quick return', function t() {
	var tc = findCase( 'n_zero' );
	var WORK = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var A = new Float64Array( 1 );
	var info;

	info = dorgqr( 3, 0, 0, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0, 1 );
	assert.equal( info, tc.INFO );
});

test( 'dorgqr: m_zero quick return', function t() {
	var tc = findCase( 'm_zero' );
	var WORK = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var A = new Float64Array( 1 );
	var info;

	info = dorgqr( 0, 0, 0, A, 1, 0, 0, TAU, 1, 0, WORK, 1, 0, 1 );
	assert.equal( info, tc.INFO );
});

test( 'dorgqr: 5x3_orthogonal (Q^T*Q = I)', function t() {
	var tc = findCase( '5x3_orthogonal' );
	var result = runDorgqr(
		new Float64Array([ 1, 4, 2, 1, 3, 2, 1, 3, 1, 2, 1, 3, 2, 4, 1 ]),
		5, 3, 3, 5
	);
	assert.equal( result.info, tc.INFO );
	assertArrayClose( result.Q, tc.Q, 1e-14, 'Q' );

	// Verify orthogonality: Q^T * Q should be close to identity
	var M = 5;
	var N = 3;
	var QtQ = new Float64Array( N * N );
	var i;
	var j;
	var k;
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			for ( k = 0; k < M; k++ ) {
				QtQ[ j * N + i ] += result.Q[ i * M + k ] * result.Q[ j * M + k ];
			}
		}
	}
	assertArrayClose( QtQ, tc.QtQ, 1e-14, 'QtQ' );
});

test( 'dorgqr: 6x4_k4', function t() {
	var tc = findCase( '6x4_k4' );
	var result = runDorgqr(
		new Float64Array([
			2, 1, 3, 1, 2, 1,
			1, 4, 2, 3, 1, 2,
			3, 2, 5, 1, 4, 1,
			1, 3, 2, 4, 1, 3
		]),
		6, 4, 4, 6
	);
	assert.equal( result.info, tc.INFO );
	assertArrayClose( result.Q, tc.Q, 1e-14, 'Q' );
});

test( 'dorgqr: 40x35_blocked (exercises blocked path with NB=32)', function t() {
	var M = 40;
	var N = 35;
	var K = 35;
	var inputA = new Float64Array( M * N );
	var expected;
	var maxErr;
	var dot;
	var i;
	var j;
	var k;

	// Reproduce the Fortran matrix: A(i,j) = sin((i+1)*7 + (j+1)*13)
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			inputA[ j * M + i ] = Math.sin( ( i + 1 ) * 7 + ( j + 1 ) * 13 );
		}
	}

	var result = runDorgqr( inputA, M, N, K, M );
	assert.equal( result.info, 0 );

	// Verify orthogonality: Q^T * Q should be close to identity
	// (Exact element values may differ from Fortran due to different
	// blocking in dgeqrf, but orthogonality must hold.)
	maxErr = 0;
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			dot = 0;
			for ( k = 0; k < M; k++ ) {
				dot += result.Q[ i * M + k ] * result.Q[ j * M + k ];
			}
			expected = ( i === j ) ? 1.0 : 0.0;
			maxErr = Math.max( maxErr, Math.abs( dot - expected ) );
		}
	}
	assert.ok( maxErr < 1e-13, 'Q^T * Q = I, max error: ' + maxErr );
});

test( 'dorgqr: 40x40_k34_blocked (blocked path with kk < N, zeroing columns)', function t() {
	var M = 40;
	var N = 40;
	var K = 34;
	var LDA = M;
	var WORK = new Float64Array( Math.max( N * 32, 1 ) );
	var TAU = new Float64Array( K );
	var A = new Float64Array( LDA * N );
	var expected;
	var maxErr;
	var info;
	var dot;
	var i;
	var j;
	var k;

	// Fill with reproducible data
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			A[ j * LDA + i ] = Math.sin( ( i + 1 ) * 7 + ( j + 1 ) * 13 );
		}
	}

	// QR factorize first K columns only
	dgeqrf( M, K, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0 );

	// Generate Q with N columns (N > K, so columns K..N-1 should be orthogonal complement)
	info = dorgqr( M, N, K, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0, N * 32 );
	assert.equal( info, 0 );

	// Verify orthogonality: Q^T * Q should be close to identity
	maxErr = 0;
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			dot = 0;
			for ( k = 0; k < M; k++ ) {
				dot += A[ i * LDA + k ] * A[ j * LDA + k ];
			}
			expected = ( i === j ) ? 1.0 : 0.0;
			maxErr = Math.max( maxErr, Math.abs( dot - expected ) );
		}
	}
	assert.ok( maxErr < 1e-13, 'Q^T * Q = I, max error: ' + maxErr );
});
