'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgeqrf = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgeqrf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function extractMatrix( A, M, N, lda ) {
	var out = [];
	var i, j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( A[ i + j * lda ] );
		}
	}
	return out;
}

test( 'dgeqrf: 3x3', function t() {
	var tc = findCase( '3x3' );
	var A = new Float64Array( 3 * 3 );
	A[ 0 ] = 2; A[ 1 ] = 1; A[ 2 ] = 3;
	A[ 3 ] = 1; A[ 4 ] = 4; A[ 5 ] = 2;
	A[ 6 ] = 3; A[ 7 ] = 2; A[ 8 ] = 5;
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 3 * 32 );
	var info = dgeqrf( 3, 3, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( extractMatrix( A, 3, 3, 3 ), tc.A, 1e-14, 'A' );
	assertArrayClose( TAU, tc.TAU, 1e-14, 'TAU' );
});

test( 'dgeqrf: 4x3', function t() {
	var tc = findCase( '4x3' );
	var A = new Float64Array( 4 * 3 );
	A[ 0 ] = 2; A[ 1 ] = 1; A[ 2 ] = 3; A[ 3 ] = 1;
	A[ 4 ] = 1; A[ 5 ] = 4; A[ 6 ] = 2; A[ 7 ] = 3;
	A[ 8 ] = 3; A[ 9 ] = 2; A[ 10 ] = 5; A[ 11 ] = 1;
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 3 * 32 );
	var info = dgeqrf( 4, 3, A, 1, 4, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( extractMatrix( A, 4, 3, 4 ), tc.A, 1e-14, 'A' );
	assertArrayClose( TAU, tc.TAU, 1e-14, 'TAU' );
});

test( 'dgeqrf: N=0', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 9 );
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 32 );
	var info = dgeqrf( 3, 0, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
});

test( 'dgeqrf: large 65x65 (blocked path)', function t() {
	var tc = findCase( 'large_65x65' );
	var M = 65;
	var N = 65;
	// Use LDA=70 to match Fortran for bit-exact results
	var LDA = 70;
	var A = new Float64Array( LDA * N );
	var i, j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			if ( i === j ) {
				A[ i + j * LDA ] = 10.0;
			} else {
				A[ i + j * LDA ] = 1.0 / ( Math.abs( i - j ) + 1 );
			}
		}
	}
	var TAU = new Float64Array( Math.min( M, N ) );
	var WORK = new Float64Array( N * 32 );
	var info = dgeqrf( M, N, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( extractMatrix( A, M, N, LDA ), tc.A, 1e-12, 'A' );
	assertArrayClose( TAU, tc.TAU, 1e-12, 'TAU' );
});
