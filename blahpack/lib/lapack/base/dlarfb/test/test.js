'use strict';

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlarfb = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarfb.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i, relErr;
	for ( i = 0; i < expected.length; i++ ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		if ( relErr > tol ) {
			throw new Error( msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
		}
	}
}

function setupV4x2( V ) {
	// LDA=6 in Fortran, so strideV1=1, strideV2=6
	V[ 0 + 0 * 6 ] = 1;
	V[ 1 + 0 * 6 ] = 0.5; V[ 1 + 1 * 6 ] = 1;
	V[ 2 + 0 * 6 ] = 0.25; V[ 2 + 1 * 6 ] = 0.5;
	V[ 3 + 0 * 6 ] = 0.125; V[ 3 + 1 * 6 ] = 0.25;
}

function setupT2x2( T ) {
	T[ 0 ] = 1.2; T[ 2 ] = -0.3;
	T[ 3 ] = 1.5;
}

function setupC4x3( C ) {
	C[ 0 + 0 * 6 ] = 1; C[ 0 + 1 * 6 ] = 2; C[ 0 + 2 * 6 ] = 3;
	C[ 1 + 0 * 6 ] = 4; C[ 1 + 1 * 6 ] = 5; C[ 1 + 2 * 6 ] = 6;
	C[ 2 + 0 * 6 ] = 7; C[ 2 + 1 * 6 ] = 8; C[ 2 + 2 * 6 ] = 9;
	C[ 3 + 0 * 6 ] = 10; C[ 3 + 1 * 6 ] = 11; C[ 3 + 2 * 6 ] = 12;
}

function extractC( C, M, N, lda ) {
	var out = [];
	var i, j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( C[ i + j * lda ] );
		}
	}
	return out;
}

test( 'dlarfb: left notrans fwd col', function t() {
	var tc = findCase( 'left_notrans_fwd_col' );
	var V = new Float64Array( 6 * 2 );
	setupV4x2( V );
	var T = new Float64Array( 4 );
	setupT2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'L', 'N', 'F', 'C', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

test( 'dlarfb: left trans fwd col', function t() {
	var tc = findCase( 'left_trans_fwd_col' );
	var V = new Float64Array( 6 * 2 );
	setupV4x2( V );
	var T = new Float64Array( 4 );
	setupT2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'L', 'T', 'F', 'C', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

test( 'dlarfb: right notrans fwd col', function t() {
	var tc = findCase( 'right_notrans_fwd_col' );
	// V: 3x2
	var V = new Float64Array( 6 * 2 );
	V[ 0 + 0 * 6 ] = 1;
	V[ 1 + 0 * 6 ] = 0.5; V[ 1 + 1 * 6 ] = 1;
	V[ 2 + 0 * 6 ] = 0.25; V[ 2 + 1 * 6 ] = 0.5;
	var T = new Float64Array( 4 );
	setupT2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'R', 'N', 'F', 'C', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

test( 'dlarfb: right trans fwd col', function t() {
	var tc = findCase( 'right_trans_fwd_col' );
	// V: 3x2
	var V = new Float64Array( 6 * 2 );
	V[ 0 + 0 * 6 ] = 1;
	V[ 1 + 0 * 6 ] = 0.5; V[ 1 + 1 * 6 ] = 1;
	V[ 2 + 0 * 6 ] = 0.25; V[ 2 + 1 * 6 ] = 0.5;
	var T = new Float64Array( 4 );
	setupT2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'R', 'T', 'F', 'C', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

// Helper for backward V: 4x2, unit triangular at bottom (rows 2,3)
function setupVBwd4x2( V ) {
	// LDA=6, column-major
	V[ 0 + 0 * 6 ] = 0.5; V[ 0 + 1 * 6 ] = 0.25;
	V[ 1 + 0 * 6 ] = 0.25; V[ 1 + 1 * 6 ] = 0.125;
	V[ 2 + 0 * 6 ] = 1.0; V[ 2 + 1 * 6 ] = 0.0;
	V[ 3 + 0 * 6 ] = 0.0; V[ 3 + 1 * 6 ] = 1.0;
}

// Helper for backward T: 2x2 lower triangular
function setupTBwd2x2( T ) {
	T[ 0 ] = 1.2;
	T[ 1 ] = -0.3; T[ 3 ] = 1.5;
}

test( 'dlarfb: left notrans bwd col', function t() {
	var tc = findCase( 'left_notrans_bwd_col' );
	var V = new Float64Array( 6 * 2 );
	setupVBwd4x2( V );
	var T = new Float64Array( 4 );
	setupTBwd2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'L', 'N', 'B', 'C', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

test( 'dlarfb: left trans bwd col', function t() {
	var tc = findCase( 'left_trans_bwd_col' );
	var V = new Float64Array( 6 * 2 );
	setupVBwd4x2( V );
	var T = new Float64Array( 4 );
	setupTBwd2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'L', 'T', 'B', 'C', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

test( 'dlarfb: right notrans bwd col', function t() {
	var tc = findCase( 'right_notrans_bwd_col' );
	// V: 3x2 with unit triangular at bottom (rows 1,2)
	var V = new Float64Array( 6 * 2 );
	V[ 0 + 0 * 6 ] = 0.5; V[ 0 + 1 * 6 ] = 0.25;
	V[ 1 + 0 * 6 ] = 1.0; V[ 1 + 1 * 6 ] = 0.0;
	V[ 2 + 0 * 6 ] = 0.0; V[ 2 + 1 * 6 ] = 1.0;
	var T = new Float64Array( 4 );
	setupTBwd2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'R', 'N', 'B', 'C', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

test( 'dlarfb: right trans bwd col', function t() {
	var tc = findCase( 'right_trans_bwd_col' );
	// V: 3x2 with unit triangular at bottom
	var V = new Float64Array( 6 * 2 );
	V[ 0 + 0 * 6 ] = 0.5; V[ 0 + 1 * 6 ] = 0.25;
	V[ 1 + 0 * 6 ] = 1.0; V[ 1 + 1 * 6 ] = 0.0;
	V[ 2 + 0 * 6 ] = 0.0; V[ 2 + 1 * 6 ] = 1.0;
	var T = new Float64Array( 4 );
	setupTBwd2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'R', 'T', 'B', 'C', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

test( 'dlarfb: M=0 quick return', function t() {
	var C = new Float64Array( [ 99 ] );
	var V = new Float64Array( 1 );
	var T = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	dlarfb( 'L', 'N', 'F', 'C', 0, 3, 2, V, 1, 1, 0, T, 1, 1, 0, C, 1, 1, 0, WORK, 1, 1, 0 );
	if ( C[ 0 ] !== 99 ) {
		throw new Error( 'C changed on M=0' );
	}
});

// =========================================================
// STOREV='R' tests: V stored row-wise
// =========================================================

// Helper for rowwise V: 2x4 (K=2, M=4), unit upper triangular in first K cols
function setupVRow2x4( V ) {
	// LDV=6 in Fortran, so strideV1=1, strideV2=6
	V[ 0 + 0 * 6 ] = 1; V[ 0 + 1 * 6 ] = 0.5; V[ 0 + 2 * 6 ] = 0.25; V[ 0 + 3 * 6 ] = 0.125;
	V[ 1 + 1 * 6 ] = 1; V[ 1 + 2 * 6 ] = 0.5; V[ 1 + 3 * 6 ] = 0.25;
}

test( 'dlarfb: left notrans fwd row', function t() {
	var tc = findCase( 'left_notrans_fwd_row' );
	var V = new Float64Array( 6 * 6 );
	setupVRow2x4( V );
	var T = new Float64Array( 4 );
	setupT2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'L', 'N', 'F', 'R', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

test( 'dlarfb: left trans fwd row', function t() {
	var tc = findCase( 'left_trans_fwd_row' );
	var V = new Float64Array( 6 * 6 );
	setupVRow2x4( V );
	var T = new Float64Array( 4 );
	setupT2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'L', 'T', 'F', 'R', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

test( 'dlarfb: right notrans fwd row', function t() {
	var tc = findCase( 'right_notrans_fwd_row' );
	// V: 2 x 3 (K=2, N=3)
	var V = new Float64Array( 6 * 6 );
	V[ 0 + 0 * 6 ] = 1; V[ 0 + 1 * 6 ] = 0.5; V[ 0 + 2 * 6 ] = 0.25;
	V[ 1 + 1 * 6 ] = 1; V[ 1 + 2 * 6 ] = 0.5;
	var T = new Float64Array( 4 );
	setupT2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'R', 'N', 'F', 'R', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

test( 'dlarfb: right trans fwd row', function t() {
	var tc = findCase( 'right_trans_fwd_row' );
	var V = new Float64Array( 6 * 6 );
	V[ 0 + 0 * 6 ] = 1; V[ 0 + 1 * 6 ] = 0.5; V[ 0 + 2 * 6 ] = 0.25;
	V[ 1 + 1 * 6 ] = 1; V[ 1 + 2 * 6 ] = 0.5;
	var T = new Float64Array( 4 );
	setupT2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'R', 'T', 'F', 'R', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

// Backward, Rowwise helpers
// V: 2 x 4, unit lower triangular at right (columns M-K..M-1 = 2,3)
function setupVBwdRow2x4( V ) {
	V[ 0 + 0 * 6 ] = 0.5; V[ 0 + 1 * 6 ] = 0.25; V[ 0 + 2 * 6 ] = 1.0; V[ 0 + 3 * 6 ] = 0.0;
	V[ 1 + 0 * 6 ] = 0.25; V[ 1 + 1 * 6 ] = 0.125; V[ 1 + 2 * 6 ] = 0.0; V[ 1 + 3 * 6 ] = 1.0;
}

test( 'dlarfb: left notrans bwd row', function t() {
	var tc = findCase( 'left_notrans_bwd_row' );
	var V = new Float64Array( 6 * 6 );
	setupVBwdRow2x4( V );
	var T = new Float64Array( 4 );
	setupTBwd2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'L', 'N', 'B', 'R', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

test( 'dlarfb: left trans bwd row', function t() {
	var tc = findCase( 'left_trans_bwd_row' );
	var V = new Float64Array( 6 * 6 );
	setupVBwdRow2x4( V );
	var T = new Float64Array( 4 );
	setupTBwd2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'L', 'T', 'B', 'R', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

test( 'dlarfb: right notrans bwd row', function t() {
	var tc = findCase( 'right_notrans_bwd_row' );
	// V: 2 x 3, unit lower triangular at right (columns N-K..N-1 = 1,2)
	var V = new Float64Array( 6 * 6 );
	V[ 0 + 0 * 6 ] = 0.5; V[ 0 + 1 * 6 ] = 1.0; V[ 0 + 2 * 6 ] = 0.0;
	V[ 1 + 0 * 6 ] = 0.25; V[ 1 + 1 * 6 ] = 0.0; V[ 1 + 2 * 6 ] = 1.0;
	var T = new Float64Array( 4 );
	setupTBwd2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'R', 'N', 'B', 'R', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

test( 'dlarfb: right trans bwd row', function t() {
	var tc = findCase( 'right_trans_bwd_row' );
	var V = new Float64Array( 6 * 6 );
	V[ 0 + 0 * 6 ] = 0.5; V[ 0 + 1 * 6 ] = 1.0; V[ 0 + 2 * 6 ] = 0.0;
	V[ 1 + 0 * 6 ] = 0.25; V[ 1 + 1 * 6 ] = 0.0; V[ 1 + 2 * 6 ] = 1.0;
	var T = new Float64Array( 4 );
	setupTBwd2x2( T );
	var C = new Float64Array( 6 * 3 );
	setupC4x3( C );
	var WORK = new Float64Array( 4 * 2 );
	dlarfb( 'R', 'T', 'B', 'R', 4, 3, 2, V, 1, 6, 0, T, 1, 2, 0, C, 1, 6, 0, WORK, 1, 4, 0 );
	assertArrayClose( extractC( C, 4, 3, 6 ), tc.C, 1e-14, 'C' );
});

