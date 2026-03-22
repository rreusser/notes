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
