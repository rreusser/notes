'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var dgehrd = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgehrd.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'dgehrd: main export is a function', function t() {
	assert.strictEqual( typeof dgehrd, 'function' );
});

test( 'dgehrd: 4x4 full range (unblocked path)', function t() {
	var tc = findCase( '4x4_full' );
	var N = 4;
	var A = new Float64Array( [
		1, 5, 9, 13,
		2, 6, 10, 14,
		3, 7, 11, 15,
		4, 8, 12, 16
	] );
	var TAU = new Float64Array( N - 1 );
	var WORK = new Float64Array( 5000 );
	var info = dgehrd( N, 1, N, A, 1, N, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( Array.from( A ), tc.A, 1e-10, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-10, 'TAU' );
});

test( 'dgehrd: 5x5 full range', function t() {
	var tc = findCase( '5x5_full' );
	var N = 5;
	var A = new Float64Array( [
		2, 1, 3, 1, 4,
		1, 4, 1, 2, 1,
		3, 1, 5, 1, 2,
		1, 2, 1, 6, 1,
		4, 1, 2, 1, 7
	] );
	var TAU = new Float64Array( N - 1 );
	var WORK = new Float64Array( 5000 );
	var info = dgehrd( N, 1, N, A, 1, N, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( Array.from( A ), tc.A, 1e-10, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-10, 'TAU' );
});

test( 'dgehrd: 4x4 partial range (ILO=2, IHI=3)', function t() {
	var tc = findCase( '4x4_partial_ilo2_ihi3' );
	var N = 4;
	var A = new Float64Array( [
		1, 0, 0, 0,
		2, 5, 8, 0,
		3, 6, 9, 0,
		4, 7, 10, 11
	] );
	var TAU = new Float64Array( N - 1 );
	var WORK = new Float64Array( 5000 );
	var info = dgehrd( N, 2, 3, A, 1, N, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( Array.from( A ), tc.A, 1e-10, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-10, 'TAU' );
});

test( 'dgehrd: N=1 (quick return)', function t() {
	var tc = findCase( 'n_one' );
	var A = new Float64Array( [ 42.0 ] );
	var TAU = new Float64Array( 0 );
	var WORK = new Float64Array( 10 );
	var info = dgehrd( 1, 1, 1, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertClose( A[ 0 ], tc.A[ 0 ], 1e-14, 'A[0]' );
});

test( 'dgehrd: N=2', function t() {
	var tc = findCase( 'n_two' );
	var A = new Float64Array( [ 3, 4, 1, 2 ] );
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( 10 );
	var info = dgehrd( 2, 1, 2, A, 1, 2, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( Array.from( A ), tc.A, 1e-10, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-10, 'TAU' );
});

test( 'dgehrd: 35x35 blocked path', function t() {
	var tc = findCase( '35x35_blocked' );
	var N = 35;
	var i;
	var j;
	var A = new Float64Array( N * N );
	// Fill with same well-conditioned diagonally dominant matrix as Fortran test
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			if ( i === j ) {
				A[ j * N + i ] = N + i + 1;
			} else {
				A[ j * N + i ] = 1.0 / ( 1 + Math.abs( i - j ) );
			}
		}
	}
	var TAU = new Float64Array( N - 1 );
	var WORK = new Float64Array( 5000 );
	var info = dgehrd( N, 1, N, A, 1, N, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( Array.from( A ), tc.A, 1e-8, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-8, 'TAU' );
});

test( 'dgehrd: ILO=IHI (nothing to reduce)', function t() {
	var tc = findCase( 'ilo_eq_ihi' );
	var N = 4;
	var A = new Float64Array( [
		1, 0, 0, 0,
		2, 5, 0, 0,
		3, 6, 9, 0,
		4, 7, 10, 11
	] );
	var TAU = new Float64Array( N - 1 );
	var WORK = new Float64Array( 5000 );
	var info = dgehrd( N, 2, 2, A, 1, N, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.strictEqual( info, tc.INFO );
	assertArrayClose( Array.from( A ), tc.A, 1e-10, 'A' );
});
