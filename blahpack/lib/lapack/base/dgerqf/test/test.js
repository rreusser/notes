

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgerqf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgerqf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

function extractMatrix( A, LDA, M, N ) {
	var out = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( A[ j * LDA + i ] );
		}
	}
	return out;
}


// TESTS //

test( 'dgerqf: 3x4 (M < N)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '3x4' );

	A = new Float64Array( 6 * 4 );
	A[ 0 * 6 + 0 ] = 2.0; A[ 1 * 6 + 0 ] = 1.0; A[ 2 * 6 + 0 ] = 3.0; A[ 3 * 6 + 0 ] = 1.0;
	A[ 0 * 6 + 1 ] = 1.0; A[ 1 * 6 + 1 ] = 4.0; A[ 2 * 6 + 1 ] = 2.0; A[ 3 * 6 + 1 ] = 3.0;
	A[ 0 * 6 + 2 ] = 3.0; A[ 1 * 6 + 2 ] = 2.0; A[ 2 * 6 + 2 ] = 5.0; A[ 3 * 6 + 2 ] = 2.0;
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 600 );

	info = dgerqf( 3, 4, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 3, 4 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgerqf: 4x3 (M > N)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '4x3' );

	A = new Float64Array( 6 * 3 );
	A[ 0 * 6 + 0 ] = 2.0; A[ 1 * 6 + 0 ] = 1.0; A[ 2 * 6 + 0 ] = 3.0;
	A[ 0 * 6 + 1 ] = 1.0; A[ 1 * 6 + 1 ] = 4.0; A[ 2 * 6 + 1 ] = 2.0;
	A[ 0 * 6 + 2 ] = 3.0; A[ 1 * 6 + 2 ] = 2.0; A[ 2 * 6 + 2 ] = 5.0;
	A[ 0 * 6 + 3 ] = 1.0; A[ 1 * 6 + 3 ] = 3.0; A[ 2 * 6 + 3 ] = 1.0;
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 600 );

	info = dgerqf( 4, 3, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 4, 3 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgerqf: 3x3 (square)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '3x3' );

	A = new Float64Array( 6 * 3 );
	A[ 0 * 6 + 0 ] = 4.0; A[ 1 * 6 + 0 ] = 1.0; A[ 2 * 6 + 0 ] = 2.0;
	A[ 0 * 6 + 1 ] = 1.0; A[ 1 * 6 + 1 ] = 3.0; A[ 2 * 6 + 1 ] = 1.0;
	A[ 0 * 6 + 2 ] = 2.0; A[ 1 * 6 + 2 ] = 1.0; A[ 2 * 6 + 2 ] = 5.0;
	TAU = new Float64Array( 3 );
	WORK = new Float64Array( 600 );

	info = dgerqf( 3, 3, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 3, 3 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgerqf: 1x1', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '1x1' );

	A = new Float64Array( 6 );
	A[ 0 ] = 7.0;
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 600 );

	info = dgerqf( 1, 1, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 1, 1 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});

test( 'dgerqf: M=0 (quick return)', function t() {
	var WORK;
	var info;
	var TAU;
	var A;

	A = new Float64Array( 1 );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );

	info = dgerqf( 0, 3, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, 0, 'INFO' );
});

test( 'dgerqf: N=0 (quick return)', function t() {
	var WORK;
	var info;
	var TAU;
	var A;

	A = new Float64Array( 1 );
	TAU = new Float64Array( 1 );
	WORK = new Float64Array( 1 );

	info = dgerqf( 3, 0, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, 0, 'INFO' );
});

test( 'dgerqf: 2x5 (wide)', function t() {
	var WORK;
	var info;
	var TAU;
	var tc;
	var A;

	tc = findCase( '2x5' );

	A = new Float64Array( 6 * 5 );
	A[ 0 * 6 + 0 ] = 1.0; A[ 1 * 6 + 0 ] = 2.0; A[ 2 * 6 + 0 ] = 3.0; A[ 3 * 6 + 0 ] = 4.0; A[ 4 * 6 + 0 ] = 5.0;
	A[ 0 * 6 + 1 ] = 6.0; A[ 1 * 6 + 1 ] = 7.0; A[ 2 * 6 + 1 ] = 8.0; A[ 3 * 6 + 1 ] = 9.0; A[ 4 * 6 + 1 ] = 10.0;
	TAU = new Float64Array( 2 );
	WORK = new Float64Array( 600 );

	info = dgerqf( 2, 5, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( extractMatrix( A, 6, 2, 5 ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( TAU ), tc.TAU, 1e-14, 'TAU' );
});
