'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zunglq = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zunglq.jsonl' ), 'utf8' ).trim().split( '\n' );
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


// TESTS //

test( 'zunglq: identity (K=0)', function t() {
	var tc = findCase( 'zunglq_identity_k0' );
	var M = tc.M;
	var N = tc.N;
	var A = new Complex128Array( M * N );
	var TAU = new Complex128Array( 1 );
	var WORK = new Complex128Array( M * 32 );
	var info;

	info = zunglq( M, N, 0, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0, M * 32 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zunglq: 3x3, K=2', function t() {
	var tc = findCase( 'zunglq_3x3_k2' );
	// LQ reflectors stored in rows
	var A = new Complex128Array( [
		1.0, 0.0,  0.0, 0.0,  0.0, 0.0,
		0.4, 0.2,  1.0, 0.0,  0.0, 0.0,
		0.1, -0.3, 0.6, 0.5,  0.0, 0.0
	]);
	var TAU = new Complex128Array( [ 1.1, 0.2, 0.9, -0.1 ] );
	var WORK = new Complex128Array( 3 * 32 );
	var info;

	info = zunglq( 3, 3, 2, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0, 3 * 32 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zunglq: 3x4, K=3 (rectangular)', function t() {
	var tc = findCase( 'zunglq_3x4_k3' );
	var A = new Complex128Array( [
		1.0, 0.0,   0.0, 0.0,   0.0, 0.0,
		0.3, 0.1,   1.0, 0.0,   0.0, 0.0,
		0.2, -0.2,  0.4, 0.3,   1.0, 0.0,
		0.1, 0.05, -0.1, 0.2,   0.5, -0.1
	]);
	var TAU = new Complex128Array( [ 1.05, 0.1, 1.15, -0.2, 0.8, 0.15 ] );
	var WORK = new Complex128Array( 3 * 32 );
	var info;

	info = zunglq( 3, 4, 3, A, 1, 3, 0, TAU, 1, 0, WORK, 1, 0, 3 * 32 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zunglq: M=0 quick return', function t() {
	var tc = findCase( 'zunglq_m0' );
	var A = new Complex128Array( 9 );
	var TAU = new Complex128Array( 1 );
	var WORK = new Complex128Array( 1 );
	var info;

	info = zunglq( 0, 3, 0, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0, 1 );
	assert.equal( info, tc.info, 'info' );
});

test( 'zunglq: 1x1, K=1', function t() {
	var tc = findCase( 'zunglq_1x1_k1' );
	var A = new Complex128Array( [ 1.0, 0.0 ] );
	var TAU = new Complex128Array( [ 0.5, 0.5 ] );
	var WORK = new Complex128Array( 32 );
	var info;

	info = zunglq( 1, 1, 1, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0, 32 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zunglq: from LQ factorization 4x4', function t() {
	var input = findCase( 'zunglq_from_lq_4x4_input' );
	var expected = findCase( 'zunglq_from_lq_4x4' );
	var M = input.M;
	var N = input.N;
	var K = input.K;
	var A = new Complex128Array( input.A );
	var TAU = new Complex128Array( input.TAU );
	var WORK = new Complex128Array( M * 32 );
	var info;

	info = zunglq( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0, M * 32 );
	assert.equal( info, expected.info, 'info' );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), expected.A, 1e-12, 'A' );
});

test( 'zunglq: blocked 40x40 (K>NB triggers blocking)', function t() {
	var input = findCase( 'zunglq_blocked_40x40_input' );
	var expected = findCase( 'zunglq_blocked_40x40' );
	var M = input.M;
	var N = input.N;
	var K = input.K;
	var A = new Complex128Array( input.A );
	var TAU = new Complex128Array( input.TAU );
	var WORK = new Complex128Array( M * 32 );
	var info;

	info = zunglq( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0, M * 32 );
	assert.equal( info, expected.info, 'info' );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), expected.A, 1e-10, 'A' );
});

test( 'zunglq: 5x8, K=5 (rectangular from LQ)', function t() {
	var input = findCase( 'zunglq_5x8_k5_input' );
	var expected = findCase( 'zunglq_5x8_k5' );
	var M = input.M;
	var N = input.N;
	var K = input.K;
	var A = new Complex128Array( input.A );
	var TAU = new Complex128Array( input.TAU );
	var WORK = new Complex128Array( M * 32 );
	var info;

	info = zunglq( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0, M * 32 );
	assert.equal( info, expected.info, 'info' );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), expected.A, 1e-12, 'A' );
});
