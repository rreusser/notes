

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dormbr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dormbr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] + ' (relErr=' + relErr + ')' );
	}
}

/**
* Extracts an MxN submatrix from a column-major flat array with leading dimension LDA,
* and returns a new flat column-major array of size M*N with leading dimension M.
*/
function extractColMajor( flat, lda, M, N ) {
	var out = new Float64Array( M * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out[ j * M + i ] = flat[ j * lda + i ];
		}
	}
	return out;
}

/**
* Creates an MxN identity matrix in column-major flat format.
*/
function eye( M, N ) {
	var out = new Float64Array( M * N );
	var k;
	for ( k = 0; k < Math.min( M, N ); k++ ) {
		out[ k * M + k ] = 1.0;
	}
	return out;
}


// TESTS //

// ===== Upper bidiagonal (M=4, N=3): M > N, uses upper bidiagonal path =====

test( 'dormbr: VECT=Q, SIDE=L, TRANS=N (upper bidiagonal, NQ>=K)', function t() {
	var gebrd = findCase( 'gebrd_4x3' );
	var tc = findCase( 'Q_L_N_upper' );
	var A = new Float64Array( gebrd.A ); // 4x3 stored col-major, LDA=4
	var TAUQ = new Float64Array( gebrd.TAUQ );
	var C = eye( 4, 4 );
	var WORK = new Float64Array( 1000 );
	// NQ = M = 4, K = 3, NQ >= K => dormqr on full A
	var info = dormbr( 'Q', 'left', 'no-transpose', 4, 4, 3, A, 1, 4, 0, TAUQ, 1, 0, C, 1, 4, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=Q, SIDE=L, TRANS=T (upper bidiagonal)', function t() {
	var gebrd = findCase( 'gebrd_4x3' );
	var tc = findCase( 'Q_L_T_upper' );
	var A = new Float64Array( gebrd.A );
	var TAUQ = new Float64Array( gebrd.TAUQ );
	var C = eye( 4, 4 );
	var WORK = new Float64Array( 1000 );
	var info = dormbr( 'Q', 'left', 'transpose', 4, 4, 3, A, 1, 4, 0, TAUQ, 1, 0, C, 1, 4, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=P, SIDE=R, TRANS=N (upper bidiagonal, NQ>=K)', function t() {
	var gebrd = findCase( 'gebrd_4x3' );
	var tc = findCase( 'P_R_N_upper' );
	var A = new Float64Array( gebrd.A );
	var TAUP = new Float64Array( gebrd.TAUP );
	var C = eye( 3, 3 );
	var WORK = new Float64Array( 1000 );
	// VECT=P, SIDE=R: NQ = N = 3, K = 3, NQ >= K is false (NQ > K is false since 3 > 3 is false)
	// NQ = 3, K = 3, so NQ > K is false, NQ > 1 is true => uses NQ<=K branch
	var info = dormbr( 'P', 'right', 'no-transpose', 3, 3, 3, A, 1, 4, 0, TAUP, 1, 0, C, 1, 3, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=P, SIDE=R, TRANS=T (upper bidiagonal)', function t() {
	var gebrd = findCase( 'gebrd_4x3' );
	var tc = findCase( 'P_R_T_upper' );
	var A = new Float64Array( gebrd.A );
	var TAUP = new Float64Array( gebrd.TAUP );
	var C = eye( 3, 3 );
	var WORK = new Float64Array( 1000 );
	var info = dormbr( 'P', 'right', 'transpose', 3, 3, 3, A, 1, 4, 0, TAUP, 1, 0, C, 1, 3, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=Q, SIDE=R, TRANS=N (upper bidiagonal)', function t() {
	var gebrd = findCase( 'gebrd_4x3' );
	var tc = findCase( 'Q_R_N_upper' );
	var A = new Float64Array( gebrd.A );
	var TAUQ = new Float64Array( gebrd.TAUQ );
	var C = eye( 4, 4 );
	var WORK = new Float64Array( 1000 );
	// VECT=Q, SIDE=R: NQ = N = 4, K = 3, NQ >= K => dormqr on full A
	var info = dormbr( 'Q', 'right', 'no-transpose', 4, 4, 3, A, 1, 4, 0, TAUQ, 1, 0, C, 1, 4, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=P, SIDE=L, TRANS=N (upper bidiagonal)', function t() {
	var gebrd = findCase( 'gebrd_4x3' );
	var tc = findCase( 'P_L_N_upper' );
	var A = new Float64Array( gebrd.A );
	var TAUP = new Float64Array( gebrd.TAUP );
	var C = eye( 3, 3 );
	var WORK = new Float64Array( 1000 );
	// VECT=P, SIDE=L: NQ = M = 3, K = 3, NQ > K is false, NQ > 1 => NQ<=K branch
	var info = dormbr( 'P', 'left', 'no-transpose', 3, 3, 3, A, 1, 4, 0, TAUP, 1, 0, C, 1, 3, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=Q, SIDE=R, TRANS=T (upper bidiagonal)', function t() {
	var gebrd = findCase( 'gebrd_4x3' );
	var tc = findCase( 'Q_R_T_upper' );
	var A = new Float64Array( gebrd.A );
	var TAUQ = new Float64Array( gebrd.TAUQ );
	var C = eye( 4, 4 );
	var WORK = new Float64Array( 1000 );
	var info = dormbr( 'Q', 'right', 'transpose', 4, 4, 3, A, 1, 4, 0, TAUQ, 1, 0, C, 1, 4, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=P, SIDE=L, TRANS=T (upper bidiagonal)', function t() {
	var gebrd = findCase( 'gebrd_4x3' );
	var tc = findCase( 'P_L_T_upper' );
	var A = new Float64Array( gebrd.A );
	var TAUP = new Float64Array( gebrd.TAUP );
	var C = eye( 3, 3 );
	var WORK = new Float64Array( 1000 );
	var info = dormbr( 'P', 'left', 'transpose', 3, 3, 3, A, 1, 4, 0, TAUP, 1, 0, C, 1, 3, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

// ===== Lower bidiagonal (M=3, N=4): M < N, uses lower bidiagonal path =====

test( 'dormbr: VECT=Q, SIDE=L, TRANS=N (lower bidiagonal, NQ<K)', function t() {
	var gebrd = findCase( 'gebrd_3x4' );
	var tc = findCase( 'Q_L_N_lower' );
	// gebrd_3x4 stores 3x4 col-major with LDA=6 in Fortran, but print_matrix uses LDA=6, M=3, N=4
	// The fixture A is 3x4 col-major flat: [ a(1,1),a(2,1),a(3,1), a(1,2),a(2,2),a(3,2), ... ]
	var A = new Float64Array( gebrd.A );
	var TAUQ = new Float64Array( gebrd.TAUQ );
	var C = eye( 3, 3 );
	var WORK = new Float64Array( 1000 );
	// VECT=Q, SIDE=L: NQ = M = 3, K = 4, NQ < K => uses A(2:NQ, 1:NQ-1) and NQ-1 reflectors
	var info = dormbr( 'Q', 'left', 'no-transpose', 3, 3, 4, A, 1, 3, 0, TAUQ, 1, 0, C, 1, 3, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=Q, SIDE=L, TRANS=T (lower bidiagonal, NQ<K)', function t() {
	var gebrd = findCase( 'gebrd_3x4' );
	var tc = findCase( 'Q_L_T_lower' );
	var A = new Float64Array( gebrd.A );
	var TAUQ = new Float64Array( gebrd.TAUQ );
	var C = eye( 3, 3 );
	var WORK = new Float64Array( 1000 );
	var info = dormbr( 'Q', 'left', 'transpose', 3, 3, 4, A, 1, 3, 0, TAUQ, 1, 0, C, 1, 3, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=P, SIDE=R, TRANS=N (lower bidiagonal, NQ>K)', function t() {
	var gebrd = findCase( 'gebrd_3x4' );
	var tc = findCase( 'P_R_N_lower' );
	var A = new Float64Array( gebrd.A );
	var TAUP = new Float64Array( gebrd.TAUP );
	var C = eye( 4, 4 );
	var WORK = new Float64Array( 1000 );
	// VECT=P, SIDE=R: NQ = N = 4, K = 3, NQ > K => dormlq on full A
	var info = dormbr( 'P', 'right', 'no-transpose', 4, 4, 3, A, 1, 3, 0, TAUP, 1, 0, C, 1, 4, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=P, SIDE=R, TRANS=T (lower bidiagonal, NQ>K)', function t() {
	var gebrd = findCase( 'gebrd_3x4' );
	var tc = findCase( 'P_R_T_lower' );
	var A = new Float64Array( gebrd.A );
	var TAUP = new Float64Array( gebrd.TAUP );
	var C = eye( 4, 4 );
	var WORK = new Float64Array( 1000 );
	var info = dormbr( 'P', 'right', 'transpose', 4, 4, 3, A, 1, 3, 0, TAUP, 1, 0, C, 1, 4, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=P, SIDE=L, TRANS=N (lower bidiagonal, NQ>K)', function t() {
	var gebrd = findCase( 'gebrd_3x4' );
	var tc = findCase( 'P_L_N_lower' );
	var A = new Float64Array( gebrd.A );
	var TAUP = new Float64Array( gebrd.TAUP );
	var C = eye( 4, 4 );
	var WORK = new Float64Array( 1000 );
	// VECT=P, SIDE=L: NQ = M = 4, K = 3, NQ > K => dormlq on full A
	var info = dormbr( 'P', 'left', 'no-transpose', 4, 4, 3, A, 1, 3, 0, TAUP, 1, 0, C, 1, 4, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=P, SIDE=L, TRANS=T (lower bidiagonal, NQ>K)', function t() {
	var gebrd = findCase( 'gebrd_3x4' );
	var tc = findCase( 'P_L_T_lower' );
	var A = new Float64Array( gebrd.A );
	var TAUP = new Float64Array( gebrd.TAUP );
	var C = eye( 4, 4 );
	var WORK = new Float64Array( 1000 );
	var info = dormbr( 'P', 'left', 'transpose', 4, 4, 3, A, 1, 3, 0, TAUP, 1, 0, C, 1, 4, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=Q, SIDE=R, TRANS=N (lower bidiagonal, NQ<K)', function t() {
	var gebrd = findCase( 'gebrd_3x4' );
	var tc = findCase( 'Q_R_N_lower' );
	var A = new Float64Array( gebrd.A );
	var TAUQ = new Float64Array( gebrd.TAUQ );
	var C = eye( 3, 3 );
	var WORK = new Float64Array( 1000 );
	// VECT=Q, SIDE=R: NQ = N = 3, K = 4, NQ < K => uses NQ-1 reflectors from A(2:,1:)
	var info = dormbr( 'Q', 'right', 'no-transpose', 3, 3, 4, A, 1, 3, 0, TAUQ, 1, 0, C, 1, 3, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=Q, SIDE=R, TRANS=T (lower bidiagonal, NQ<K)', function t() {
	var gebrd = findCase( 'gebrd_3x4' );
	var tc = findCase( 'Q_R_T_lower' );
	var A = new Float64Array( gebrd.A );
	var TAUQ = new Float64Array( gebrd.TAUQ );
	var C = eye( 3, 3 );
	var WORK = new Float64Array( 1000 );
	var info = dormbr( 'Q', 'right', 'transpose', 3, 3, 4, A, 1, 3, 0, TAUQ, 1, 0, C, 1, 3, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

// ===== Non-identity C tests =====

test( 'dormbr: VECT=Q, SIDE=L, TRANS=N, non-identity C (upper bidiagonal)', function t() {
	var gebrd = findCase( 'gebrd_4x3' );
	var tc = findCase( 'Q_L_N_nonident' );
	var A = new Float64Array( gebrd.A );
	var TAUQ = new Float64Array( gebrd.TAUQ );
	// C is 4x2: [1,3,-1,2, 2,0,4,-1] column-major
	var C = new Float64Array( [ 1, 3, -1, 2, 2, 0, 4, -1 ] );
	var WORK = new Float64Array( 1000 );
	var info = dormbr( 'Q', 'left', 'no-transpose', 4, 2, 3, A, 1, 4, 0, TAUQ, 1, 0, C, 1, 4, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

test( 'dormbr: VECT=P, SIDE=R, TRANS=N, non-identity C (lower bidiagonal)', function t() {
	var gebrd = findCase( 'gebrd_3x4' );
	var tc = findCase( 'P_R_N_lower_nonident' );
	var A = new Float64Array( gebrd.A );
	var TAUP = new Float64Array( gebrd.TAUP );
	// C is 3x4: [1,3,-1, 2,0,4, -1,2,1, 0,1,-2] column-major
	var C = new Float64Array( [ 1, 3, -1, 2, 0, 4, -1, 2, 1, 0, 1, -2 ] );
	var WORK = new Float64Array( 1000 );
	var info = dormbr( 'P', 'right', 'no-transpose', 3, 4, 3, A, 1, 3, 0, TAUP, 1, 0, C, 1, 3, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});

// ===== Edge cases =====

test( 'dormbr: M=0 quick return', function t() {
	var A = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var C = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info = dormbr( 'Q', 'left', 'no-transpose', 0, 3, 0, A, 1, 1, 0, TAU, 1, 0, C, 1, 1, 0, WORK, 1, 0, 1 );
	assert.equal( info, 0 );
});

test( 'dormbr: N=0 quick return', function t() {
	var A = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var C = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info = dormbr( 'Q', 'left', 'no-transpose', 3, 0, 0, A, 1, 1, 0, TAU, 1, 0, C, 1, 1, 0, WORK, 1, 0, 1 );
	assert.equal( info, 0 );
});

test( 'dormbr: K=0, VECT=Q (dormqr with K=0, C unchanged)', function t() {
	var tc = findCase( 'k_zero' );
	var A = new Float64Array( 16 );
	var TAU = new Float64Array( 1 );
	var C = eye( 4, 4 );
	var WORK = new Float64Array( 1000 );
	// NQ = 4, K = 0, NQ >= K => dormqr with K=0, should leave C unchanged
	var info = dormbr( 'Q', 'left', 'no-transpose', 4, 4, 0, A, 1, 4, 0, TAU, 1, 0, C, 1, 4, 0, WORK, 1, 0, 1000 );
	assert.equal( info, 0 );
	assertArrayClose( C, tc.C, 1e-14, 'C' );
});
