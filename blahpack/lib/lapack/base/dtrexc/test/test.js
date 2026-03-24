'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtrexc = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtrexc.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dtrexc: swap 1x1 forward', function t() {
	var tc = findCase( 'swap 1x1 forward' );
	var N = 4;
	var T = buildSchurMatrix([
		[0,0,4], [0,1,1], [0,2,0.5], [0,3,0.2],
		[1,1,3], [1,2,0.8], [1,3,0.3],
		[2,2,2], [2,3,0.6],
		[3,3,1]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 1, 4, WORK, 1, 0 );

	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: N=1 quick return', function t() {
	var T = new Float64Array([ 5.0 ]);
	var Q = new Float64Array([ 1.0 ]);
	var WORK = new Float64Array( 1 );

	var r = dtrexc( 'update', 1, T, 1, 1, 0, Q, 1, 1, 0, 1, 1, WORK, 1, 0 );
	assert.strictEqual( r.info, 0, 'info' );
});

test( 'dtrexc: swap 1x1 backward', function t() {
	var tc = findCase( 'swap 1x1 backward' );
	var N = 4;
	var T = buildSchurMatrix([
		[0,0,4], [0,1,1], [0,2,0.5], [0,3,0.2],
		[1,1,3], [1,2,0.8], [1,3,0.3],
		[2,2,2], [2,3,0.6],
		[3,3,1]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 4, 1, WORK, 1, 0 );

	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: swap 1x1 forward, COMPQ=N', function t() {
	var tc = findCase( 'swap 1x1 forward compq_N' );
	var N = 4;
	var T = buildSchurMatrix([
		[0,0,4], [0,1,1], [0,2,0.5], [0,3,0.2],
		[1,1,3], [1,2,0.8], [1,3,0.3],
		[2,2,2], [2,3,0.6],
		[3,3,1]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var r = dtrexc( 'none', N, T, 1, N, 0, Q, 1, N, 0, 1, 4, WORK, 1, 0 );

	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	// Q should be unchanged (identity) since COMPQ='none'
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q unchanged' );
});

test( 'dtrexc: forward 2x2 block', function t() {
	var tc = findCase( 'forward 2x2 block' );
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

	var r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 1, 4, WORK, 1, 0 );

	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: backward 2x2 block', function t() {
	var tc = findCase( 'backward 2x2 block' );
	var N = 5;
	var T = buildSchurMatrix([
		[0,0,5], [0,1,0.5], [0,2,0.2], [0,3,0.1], [0,4,0.3],
		[1,1,1], [1,2,0.6], [1,3,0.4], [1,4,0.15],
		[2,2,0.5], [2,3,0.9], [2,4,0.2],
		[3,3,3], [3,4,2],
		[4,3,-2], [4,4,3]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 4, 1, WORK, 1, 0 );

	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: forward 1x1 across 2x2', function t() {
	var tc = findCase( 'forward 1x1 across 2x2' );
	var N = 5;
	var T = buildSchurMatrix([
		[0,0,5],
		[0,1,0.5], [0,2,0.2], [0,3,0.1], [0,4,0.3],
		[1,1,3], [1,2,2], [1,3,0.6], [1,4,0.4],
		[2,1,-2], [2,2,3], [2,3,0.8], [2,4,0.15],
		[3,3,1], [3,4,0.9],
		[4,4,0.5]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 1, 5, WORK, 1, 0 );

	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: backward 1x1 across 2x2', function t() {
	var tc = findCase( 'backward 1x1 across 2x2' );
	var N = 5;
	var T = buildSchurMatrix([
		[0,0,1], [0,1,0.5], [0,2,0.2], [0,3,0.1], [0,4,0.3],
		[1,1,3], [1,2,2], [1,3,0.6], [1,4,0.4],
		[2,1,-2], [2,2,3], [2,3,0.8], [2,4,0.15],
		[3,3,5], [3,4,0.9],
		[4,4,0.5]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 5, 1, WORK, 1, 0 );

	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: IFST==ILST (no-op)', function t() {
	var N = 3;
	var T = buildSchurMatrix([
		[0,0,4], [0,1,1], [0,2,0.5],
		[1,1,3], [1,2,0.8],
		[2,2,2]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 2, 2, WORK, 1, 0 );

	assert.strictEqual( r.info, 0, 'info' );
});

test( 'dtrexc: IFST points to second row of 2x2 block', function t() {
	var tc = findCase( 'ifst_adjusted_2x2' );
	var N = 4;
	var T = buildSchurMatrix([
		[0,0,3], [0,1,2], [0,2,0.5], [0,3,0.2],
		[1,0,-2], [1,1,3], [1,2,0.8], [1,3,0.3],
		[2,2,5], [2,3,0.6],
		[3,3,1]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 2, 4, WORK, 1, 0 );

	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});

test( 'dtrexc: backward 1x1, COMPQ=N', function t() {
	var tc = findCase( 'backward 1x1 compq_N' );
	var N = 4;
	var T = buildSchurMatrix([
		[0,0,4], [0,1,1], [0,2,0.5], [0,3,0.2],
		[1,1,3], [1,2,0.8], [1,3,0.3],
		[2,2,2], [2,3,0.6],
		[3,3,1]
	], N);
	var Q = eye( N );
	var WORK = new Float64Array( N );

	var r = dtrexc( 'none', N, T, 1, N, 0, Q, 1, N, 0, 3, 1, WORK, 1, 0 );

	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
});

test( 'dtrexc: forward 2x2 across 2x2', function t() {
	var tc = findCase( 'forward 2x2 across 2x2' );
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

	var r = dtrexc( 'update', N, T, 1, N, 0, Q, 1, N, 0, 1, 4, WORK, 1, 0 );

	assert.strictEqual( r.info, tc.info, 'info' );
	assertArrayClose( Array.from( T ), tc.T, 1e-12, 'T' );
	assertArrayClose( Array.from( Q ), tc.Q, 1e-12, 'Q' );
});
