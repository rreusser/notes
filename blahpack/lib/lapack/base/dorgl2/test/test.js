'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgelq2 = require( '../../dgelq2/lib/base.js' );
var dorgl2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dorgl2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dorgl2: 3x4_k3 (M < N, full K=M from LQ)', function t() {
	var tc = findCase( '3x4_k3' );
	var M = 3;
	var N = 4;
	var K = 3;
	var A = new Float64Array([
		2.0, 1.0, 3.0,
		1.0, 4.0, 2.0,
		3.0, 2.0, 5.0,
		1.0, 3.0, 2.0
	]);
	var TAU = new Float64Array( K );
	var WORK = new Float64Array( N );
	var info;

	// Column-major: strideA1=1, strideA2=M
	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorgl2( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
});

test( 'dorgl2: 3x3_k3 (square, full K=M from LQ)', function t() {
	var tc = findCase( '3x3_k3' );
	var M = 3;
	var N = 3;
	var K = 3;
	var A = new Float64Array([
		4.0, 1.0, 2.0,
		1.0, 3.0, 1.0,
		2.0, 1.0, 5.0
	]);
	var TAU = new Float64Array( K );
	var WORK = new Float64Array( N );
	var info;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorgl2( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
});

test( 'dorgl2: 2x5_k1 (K < M, partial generation)', function t() {
	var tc = findCase( '2x5_k1' );
	var M = 2;
	var N = 5;
	var K = 1;
	var A = new Float64Array([
		1.0, 6.0,
		2.0, 7.0,
		3.0, 8.0,
		4.0, 9.0,
		5.0, 10.0
	]);
	var TAU = new Float64Array( M );
	var WORK = new Float64Array( N );
	var info;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorgl2( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
});

test( 'dorgl2: k0_identity (K=0 produces identity)', function t() {
	var tc = findCase( 'k0_identity' );
	var M = 3;
	var N = 3;
	var K = 0;
	var A = new Float64Array([
		9.0, 9.0, 9.0,
		9.0, 9.0, 9.0,
		9.0, 9.0, 9.0
	]);
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( N );
	var info;

	info = dorgl2( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
});

test( 'dorgl2: m0_quick (M=0 quick return)', function t() {
	var tc = findCase( 'm0_quick' );
	var A = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dorgl2( 0, 4, 0, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
});

test( 'dorgl2: 1x1_k1', function t() {
	var tc = findCase( '1x1_k1' );
	var M = 1;
	var N = 1;
	var K = 1;
	var A = new Float64Array([ 7.0 ]);
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorgl2( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
});

test( 'dorgl2: 1x4_k1 (single row)', function t() {
	var tc = findCase( '1x4_k1' );
	var M = 1;
	var N = 4;
	var K = 1;
	// LDA=1 for 1xN, column-major: each column is 1 element
	var A = new Float64Array([ 1.0, 2.0, 3.0, 4.0 ]);
	var TAU = new Float64Array( 1 );
	var WORK = new Float64Array( N );
	var info;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorgl2( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
});

test( 'dorgl2: 2x5_k2 (full K=M from LQ)', function t() {
	var tc = findCase( '2x5_k2' );
	var M = 2;
	var N = 5;
	var K = 2;
	var A = new Float64Array([
		1.0, 6.0,
		2.0, 7.0,
		3.0, 8.0,
		4.0, 9.0,
		5.0, 10.0
	]);
	var TAU = new Float64Array( K );
	var WORK = new Float64Array( N );
	var info;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorgl2( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
});

test( 'dorgl2: 3x4_k2 (K < M, partial generation)', function t() {
	var tc = findCase( '3x4_k2' );
	var M = 3;
	var N = 4;
	var K = 2;
	var A = new Float64Array([
		2.0, 1.0, 3.0,
		1.0, 4.0, 2.0,
		3.0, 2.0, 5.0,
		1.0, 3.0, 2.0
	]);
	var TAU = new Float64Array( M );
	var WORK = new Float64Array( N );
	var info;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	info = dorgl2( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	assert.equal( info, tc.info );
	assertArrayClose( A, tc.A, 1e-14, 'A' );
});

test( 'dorgl2: verifies Q*Q^T = I for 3x4_k3', function t() {
	var M = 3;
	var N = 4;
	var K = 3;
	var A = new Float64Array([
		2.0, 1.0, 3.0,
		1.0, 4.0, 2.0,
		3.0, 2.0, 5.0,
		1.0, 3.0, 2.0
	]);
	var TAU = new Float64Array( K );
	var WORK = new Float64Array( N );
	var QQT;
	var i;
	var j;
	var k;
	var sum;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	dorgl2( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	// Compute Q * Q^T manually
	QQT = new Float64Array( M * M );
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < M; j++ ) {
			sum = 0.0;
			for ( k = 0; k < N; k++ ) {
				// A is column-major: A(i,k) = A[k*M + i]
				sum += A[ k * M + i ] * A[ k * M + j ];
			}
			QQT[ j * M + i ] = sum;
		}
	}

	// Should be identity
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < M; j++ ) {
			if ( i === j ) {
				assertClose( QQT[ j * M + i ], 1.0, 1e-14, 'QQT[' + i + ',' + j + ']' );
			} else {
				assert.ok( Math.abs( QQT[ j * M + i ] ) < 1e-14, 'QQT[' + i + ',' + j + '] should be ~0' );
			}
		}
	}
});

test( 'dorgl2: verifies Q*Q^T = I for 3x3_k3 (square)', function t() {
	var M = 3;
	var N = 3;
	var K = 3;
	var A = new Float64Array([
		4.0, 1.0, 2.0,
		1.0, 3.0, 1.0,
		2.0, 1.0, 5.0
	]);
	var TAU = new Float64Array( K );
	var WORK = new Float64Array( N );
	var QQT;
	var i;
	var j;
	var k;
	var sum;

	dgelq2( M, N, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );
	dorgl2( M, N, K, A, 1, M, 0, TAU, 1, 0, WORK, 1, 0 );

	QQT = new Float64Array( M * M );
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < M; j++ ) {
			sum = 0.0;
			for ( k = 0; k < N; k++ ) {
				sum += A[ k * M + i ] * A[ k * M + j ];
			}
			QQT[ j * M + i ] = sum;
		}
	}

	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < M; j++ ) {
			if ( i === j ) {
				assertClose( QQT[ j * M + i ], 1.0, 1e-14, 'QQT[' + i + ',' + j + ']' );
			} else {
				assert.ok( Math.abs( QQT[ j * M + i ] ) < 1e-14, 'QQT[' + i + ',' + j + '] should be ~0' );
			}
		}
	}
});
