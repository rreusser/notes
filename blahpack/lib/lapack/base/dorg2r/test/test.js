'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgeqr2 = require( '../../dgeqr2/lib/base.js' );
var dorg2r = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dorg2r.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Helper: set up a column-major matrix from row-by-row values,
* run dgeqr2, then run dorg2r, return the Q matrix as a flat column-major array.
*
* @param {number} M - rows
* @param {number} N - columns
* @param {number} K - number of reflectors to apply
* @param {Array} values - M*N values in row-major order
* @returns {Object} { Q: Float64Array (col-major flat), info: integer }
*/
function computeQ( M, N, K, values ) {
	var WORK;
	var info;
	var TAU;
	var A;
	var Q;
	var i;
	var j;

	// Build column-major A from row-major input values
	A = new Float64Array( M * N );
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			A[ j * M + i ] = values[ i * N + j ];
		}
	}
	TAU = new Float64Array( Math.min( M, N ) );
	WORK = new Float64Array( N );

	// Compute QR factorization
	info = dgeqr2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'dgeqr2 info' );

	// Generate Q from reflectors
	info = dorg2r( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	// Extract column-major flat array for comparison
	Q = new Float64Array( M * N );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			Q[ j * M + i ] = A[ j * M + i ];
		}
	}
	return { Q: Q, info: info };
}


// TESTS //

test( 'dorg2r: 4x3_k3', function t() {
	var tc = findCase( '4x3_k3' );
	var result = computeQ( 4, 3, 3, [
		2, 1, 3,
		1, 4, 2,
		3, 2, 5,
		1, 3, 1
	]);
	assert.equal( result.info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( result.Q ), tc.Q, 1e-14, 'Q' );
});

test( 'dorg2r: 3x3_k3', function t() {
	var tc = findCase( '3x3_k3' );
	var result = computeQ( 3, 3, 3, [
		4, 1, 2,
		3, 2, 1,
		1, 5, 3
	]);
	assert.equal( result.info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( result.Q ), tc.Q, 1e-14, 'Q' );
});

test( 'dorg2r: 4x2_k1 (K < N, partial reflectors)', function t() {
	var tc = findCase( '4x2_k1' );
	var WORK;
	var info;
	var TAU;
	var A;
	var i;
	var j;

	// Build column-major A
	A = new Float64Array( 4 * 2 );
	var vals = [
		1, 2,
		3, 4,
		5, 6,
		7, 8
	];
	for ( i = 0; i < 4; i++ ) {
		for ( j = 0; j < 2; j++ ) {
			A[ j * 4 + i ] = vals[ i * 2 + j ];
		}
	}
	TAU = new Float64Array( 2 );
	WORK = new Float64Array( 2 );

	// Compute QR with full N=2 reflectors
	info = dgeqr2( 4, 2, A, 1, 4, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'dgeqr2 info' );

	// But only apply K=1 reflector in dorg2r
	info = dorg2r( 4, 2, 1, A, 1, 4, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( A ), tc.Q, 1e-14, 'Q' );
});

test( 'dorg2r: k_zero (identity)', function t() {
	var tc = findCase( 'k_zero' );
	var WORK;
	var info;
	var TAU;
	var A;

	// K=0 means no reflectors — should produce identity columns
	A = new Float64Array( 3 * 2 );
	A[ 0 ] = 99; A[ 3 ] = 88;
	A[ 1 ] = 77; A[ 4 ] = 66;
	A[ 2 ] = 55; A[ 5 ] = 44;
	TAU = new Float64Array( 2 );
	WORK = new Float64Array( 2 );

	info = dorg2r( 3, 2, 0, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( A ), tc.Q, 1e-14, 'Q' );
});

test( 'dorg2r: n_zero (quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var WORK;
	var info;
	var TAU;
	var A;

	A = new Float64Array( 3 );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );

	info = dorg2r( 3, 0, 0, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.INFO, 'INFO' );
});

test( 'dorg2r: m_zero (quick return)', function t() {
	var tc = findCase( 'm_zero' );
	var WORK;
	var info;
	var TAU;
	var A;

	A = new Float64Array( 1 );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );

	info = dorg2r( 0, 0, 0, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.INFO, 'INFO' );
});

test( 'dorg2r: 5x3_orthogonal (verify Q^T Q = I)', function t() {
	var tc = findCase( '5x3_orthogonal' );
	var result = computeQ( 5, 3, 3, [
		1, 2, 1,
		4, 1, 3,
		2, 3, 2,
		1, 1, 4,
		3, 2, 1
	]);
	var QtQ;
	var Q;
	var M;
	var N;
	var i;
	var j;
	var k;

	assert.equal( result.info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( result.Q ), tc.Q, 1e-14, 'Q' );

	// Verify orthogonality: Q^T * Q should be identity
	Q = result.Q;
	M = 5;
	N = 3;
	QtQ = new Float64Array( N * N );
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			for ( k = 0; k < M; k++ ) {
				QtQ[ j * N + i ] += Q[ i * M + k ] * Q[ j * M + k ];
			}
		}
	}
	// Check QtQ against fixture
	assertArrayClose( Array.from( QtQ ), tc.QtQ, 1e-14, 'QtQ' );

	// Also verify each diagonal entry is 1 and off-diags are ~0
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			if ( i === j ) {
				assertClose( QtQ[ j * N + i ], 1.0, 1e-14, 'QtQ diagonal [' + i + '][' + j + ']' );
			} else {
				assert.ok( Math.abs( QtQ[ j * N + i ] ) < 1e-14, 'QtQ off-diagonal [' + i + '][' + j + '] should be ~0, got ' + QtQ[ j * N + i ] );
			}
		}
	}
});

test( 'dorg2r: 6x4_k4', function t() {
	var tc = findCase( '6x4_k4' );
	var result = computeQ( 6, 4, 4, [
		2, 1, 3, 1,
		1, 4, 2, 3,
		3, 2, 5, 2,
		1, 3, 1, 4,
		2, 1, 4, 1,
		1, 2, 1, 3
	]);
	assert.equal( result.info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( result.Q ), tc.Q, 1e-14, 'Q' );
});
