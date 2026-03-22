'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgeqr2 = require( './../lib/base.js' );

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgeqr2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dgeqr2: 3x2', function t() {
	var tc = findCase( '3x2' );
	// LDA=6 in Fortran, pack as LDA=3 in JS
	var A = new Float64Array( 3 * 2 );
	A[ 0 ] = 1; A[ 1 ] = 3; A[ 2 ] = 5;
	A[ 3 ] = 2; A[ 4 ] = 4; A[ 5 ] = 6;
	var TAU = new Float64Array( 2 );
	var WORK = new Float64Array( 2 );
	var info = dgeqr2( 3, 2, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( extractMatrix( A, 3, 2, 3 ), tc.A, 1e-14, 'A' );
	assertArrayClose( TAU, tc.TAU, 1e-14, 'TAU' );
});

test( 'dgeqr2: 2x2', function t() {
	var tc = findCase( '2x2' );
	var A = new Float64Array( 2 * 2 );
	A[ 0 ] = 4; A[ 1 ] = 3;
	A[ 2 ] = 1; A[ 3 ] = 2;
	var TAU = new Float64Array( 2 );
	var WORK = new Float64Array( 2 );
	var info = dgeqr2( 2, 2, A, 1, 2, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( extractMatrix( A, 2, 2, 2 ), tc.A, 1e-14, 'A' );
	assertArrayClose( TAU, tc.TAU, 1e-14, 'TAU' );
});

test( 'dgeqr2: N=0', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 2 );
	var TAU = new Float64Array( 2 );
	var WORK = new Float64Array( 2 );
	var info = dgeqr2( 2, 0, A, 1, 2, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
});

test( 'dgeqr2: M=0', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Float64Array( 2 );
	var TAU = new Float64Array( 2 );
	var WORK = new Float64Array( 2 );
	var info = dgeqr2( 0, 2, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
});

test( 'dgeqr2: 4x3', function t() {
	var tc = findCase( '4x3' );
	var A = new Float64Array( 4 * 3 );
	A[ 0 ] = 2; A[ 1 ] = 1; A[ 2 ] = 3; A[ 3 ] = 1;
	A[ 4 ] = 1; A[ 5 ] = 4; A[ 6 ] = 2; A[ 7 ] = 3;
	A[ 8 ] = 3; A[ 9 ] = 2; A[ 10 ] = 5; A[ 11 ] = 1;
	var TAU = new Float64Array( 3 );
	var WORK = new Float64Array( 3 );
	var info = dgeqr2( 4, 3, A, 1, 4, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( extractMatrix( A, 4, 3, 4 ), tc.A, 1e-14, 'A' );
	assertArrayClose( TAU, tc.TAU, 1e-14, 'TAU' );
});
