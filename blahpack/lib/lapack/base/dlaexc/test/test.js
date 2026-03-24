'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlaexc = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlaexc.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Build an NxN column-major matrix and identity Q for dlaexc tests.
*/
function buildSchurMatrix( entries, N ) {
	var T = new Float64Array( N * N );
	var i;
	for ( i = 0; i < entries.length; i++ ) {
		T[ entries[ i ][ 0 ] + entries[ i ][ 1 ] * N ] = entries[ i ][ 2 ];
	}
	return T;
}

function eye( N ) {
	var Q = new Float64Array( N * N );
	var i;
	for ( i = 0; i < N; i++ ) {
		Q[ i + i * N ] = 1.0;
	}
	return Q;
}


// TESTS //

test( 'dlaexc: N1=1, N2=1, WANTQ=true', function t() {
	var tc = findCase( 'n1_1_n2_1_wantq_true' );
	var N = 4;
	var T = buildSchurMatrix([
		[0,0,4], [0,1,1], [0,2,0.5], [0,3,0.2],
		[1,1,3], [1,2,0.8], [1,3,0.3],
		[2,2,2], [2,3,0.6],
		[3,3,1]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var info = dlaexc( true, N, T, 1, N, 0, Q, 1, N, 0, 2, 1, 1, WORK, 1, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dlaexc: N1=1, N2=1, WANTQ=false', function t() {
	var tc = findCase( 'n1_1_n2_1_wantq_false' );
	var N = 4;
	var T = buildSchurMatrix([
		[0,0,4], [0,1,1], [0,2,0.5], [0,3,0.2],
		[1,1,3], [1,2,0.8], [1,3,0.3],
		[2,2,2], [2,3,0.6],
		[3,3,1]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var info = dlaexc( false, N, T, 1, N, 0, Q, 1, N, 0, 2, 1, 1, WORK, 1, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	// Q should be unchanged (identity)
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q unchanged' );
});

test( 'dlaexc: N1=1, N2=2, WANTQ=true', function t() {
	var tc = findCase( 'n1_1_n2_2_wantq_true' );
	var N = 5;
	var T = buildSchurMatrix([
		[0,0,5],
		[0,1,1], [0,2,0.3], [0,3,0.2], [0,4,0.1],
		[1,1,3], [1,2,2], [1,3,0.5], [1,4,0.4],
		[2,1,-2], [2,2,3], [2,3,0.7], [2,4,0.6],
		[3,3,1], [3,4,0.9],
		[4,4,0.5]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var info = dlaexc( true, N, T, 1, N, 0, Q, 1, N, 0, 1, 1, 2, WORK, 1, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dlaexc: N1=1, N2=2, WANTQ=false', function t() {
	var tc = findCase( 'n1_1_n2_2_wantq_false' );
	var N = 5;
	var T = buildSchurMatrix([
		[0,0,5],
		[0,1,1], [0,2,0.3], [0,3,0.2], [0,4,0.1],
		[1,1,3], [1,2,2], [1,3,0.5], [1,4,0.4],
		[2,1,-2], [2,2,3], [2,3,0.7], [2,4,0.6],
		[3,3,1], [3,4,0.9],
		[4,4,0.5]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var info = dlaexc( false, N, T, 1, N, 0, Q, 1, N, 0, 1, 1, 2, WORK, 1, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
});

test( 'dlaexc: N1=2, N2=1, WANTQ=true', function t() {
	var tc = findCase( 'n1_2_n2_1_wantq_true' );
	var N = 5;
	var T = buildSchurMatrix([
		[0,0,3], [0,1,2], [0,2,0.5], [0,3,0.2], [0,4,0.1],
		[1,0,-2], [1,1,3], [1,2,0.8], [1,3,0.3], [1,4,0.15],
		[2,2,5], [2,3,0.6], [2,4,0.4],
		[3,3,1], [3,4,0.9],
		[4,4,0.5]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var info = dlaexc( true, N, T, 1, N, 0, Q, 1, N, 0, 1, 2, 1, WORK, 1, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dlaexc: N1=2, N2=1, WANTQ=false', function t() {
	var tc = findCase( 'n1_2_n2_1_wantq_false' );
	var N = 5;
	var T = buildSchurMatrix([
		[0,0,3], [0,1,2], [0,2,0.5], [0,3,0.2], [0,4,0.1],
		[1,0,-2], [1,1,3], [1,2,0.8], [1,3,0.3], [1,4,0.15],
		[2,2,5], [2,3,0.6], [2,4,0.4],
		[3,3,1], [3,4,0.9],
		[4,4,0.5]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var info = dlaexc( false, N, T, 1, N, 0, Q, 1, N, 0, 1, 2, 1, WORK, 1, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
});

test( 'dlaexc: N1=2, N2=2, WANTQ=true', function t() {
	var tc = findCase( 'n1_2_n2_2_wantq_true' );
	var N = 6;
	var T = buildSchurMatrix([
		[0,0,4], [0,1,1], [0,2,0.5], [0,3,0.3], [0,4,0.2], [0,5,0.1],
		[1,0,-1], [1,1,4], [1,2,0.8], [1,3,0.4], [1,4,0.25], [1,5,0.15],
		[2,2,2], [2,3,3], [2,4,0.6], [2,5,0.35],
		[3,2,-3], [3,3,2], [3,4,0.7], [3,5,0.45],
		[4,4,1], [4,5,0.9],
		[5,5,0.5]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var info = dlaexc( true, N, T, 1, N, 0, Q, 1, N, 0, 1, 2, 2, WORK, 1, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dlaexc: N1=2, N2=2, WANTQ=false', function t() {
	var tc = findCase( 'n1_2_n2_2_wantq_false' );
	var N = 6;
	var T = buildSchurMatrix([
		[0,0,4], [0,1,1], [0,2,0.5], [0,3,0.3], [0,4,0.2], [0,5,0.1],
		[1,0,-1], [1,1,4], [1,2,0.8], [1,3,0.4], [1,4,0.25], [1,5,0.15],
		[2,2,2], [2,3,3], [2,4,0.6], [2,5,0.35],
		[3,2,-3], [3,3,2], [3,4,0.7], [3,5,0.45],
		[4,4,1], [4,5,0.9],
		[5,5,0.5]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var info = dlaexc( false, N, T, 1, N, 0, Q, 1, N, 0, 1, 2, 2, WORK, 1, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
});

test( 'dlaexc: quick return N=0', function t() {
	var info = dlaexc( true, 0, new Float64Array(0), 1, 0, 0, new Float64Array(0), 1, 0, 0, 1, 1, 1, new Float64Array(0), 1, 0 );
	assert.strictEqual( info, 0, 'info' );
});

test( 'dlaexc: quick return n1=0', function t() {
	var T = new Float64Array( 16 );
	var Q = new Float64Array( 16 );
	var WORK = new Float64Array( 4 );
	var info = dlaexc( true, 4, T, 1, 4, 0, Q, 1, 4, 0, 1, 0, 1, WORK, 1, 0 );
	assert.strictEqual( info, 0, 'info' );
});

test( 'dlaexc: N1=1, N2=1 boundary (end of matrix)', function t() {
	var tc = findCase( 'n1_1_n2_1_boundary' );
	var N = 3;
	var T = buildSchurMatrix([
		[0,0,5], [0,1,1], [0,2,0.5],
		[1,1,3], [1,2,0.8],
		[2,2,1]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var info = dlaexc( true, N, T, 1, N, 0, Q, 1, N, 0, 2, 1, 1, WORK, 1, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dlaexc: N1=1, N2=1 at start (j1=1)', function t() {
	var tc = findCase( 'n1_1_n2_1_start' );
	var N = 3;
	var T = buildSchurMatrix([
		[0,0,5], [0,1,1], [0,2,0.5],
		[1,1,3], [1,2,0.8],
		[2,2,1]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var info = dlaexc( true, N, T, 1, N, 0, Q, 1, N, 0, 1, 1, 1, WORK, 1, 0 );

	assert.strictEqual( info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});
