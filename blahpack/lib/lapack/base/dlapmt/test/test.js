/**
* @license Apache-2.0
*
* Copyright (c) 2025 The Stdlib Authors.
*/

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dlapmt = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlapmt.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Load a dense M-by-N matrix (column-major) into a Float64Array with LDA stride.
*
* @param {Array} data - dense column-major data (M*N elements)
* @param {number} M - rows
* @param {number} N - columns
* @param {number} LDA - leading dimension (>= M)
* @returns {Float64Array} buffer of size LDA*N
*/
function loadMatrix( data, M, N, LDA ) {
	var A = new Float64Array( LDA * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			A[ j * LDA + i ] = data[ j * M + i ];
		}
	}
	return A;
}

/**
* Extract M-by-N submatrix from flat column-major array with leading dim LDA.
*
* @returns {Array} extracted values in column-major order (M*N elements)
*/
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

/**
* Extract M-by-N submatrix from fixture's X (which has LDA=MMAX=4).
*/
function extractFixtureX( tc ) {
	var LDA = 4; // MMAX in Fortran test
	var M = tc.M;
	var N = tc.N;
	return extractMatrix( tc.X, LDA, M, N );
}


// TESTS //

test( 'dlapmt: forward permutation 3x4', function t() {
	var tc = findCase( 'forward_3x4' );
	var M = 3;
	var N = 4;
	var LDA = 4;

	// Input matrix (column-major, M=3, LDA=4):
	// col 0: [1, 2, 3], col 1: [5, 6, 7], col 2: [9, 10, 11], col 3: [13, 14, 15]
	var Xdata = [ 1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 14, 15 ];
	var X = loadMatrix( Xdata, M, N, LDA );

	// K = [3, 1, 4, 2] in Fortran (1-based) -> [2, 0, 3, 1] in JS (0-based)
	var K = new Int32Array([ 2, 0, 3, 1 ]);

	dlapmt( true, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X' );
});

test( 'dlapmt: backward permutation 3x4', function t() {
	var tc = findCase( 'backward_3x4' );
	var M = 3;
	var N = 4;
	var LDA = 4;

	var Xdata = [ 1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 14, 15 ];
	var X = loadMatrix( Xdata, M, N, LDA );

	// K = [3, 1, 4, 2] -> [2, 0, 3, 1] (0-based)
	var K = new Int32Array([ 2, 0, 3, 1 ]);

	dlapmt( false, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X' );
});

test( 'dlapmt: identity permutation 2x3', function t() {
	var tc = findCase( 'identity_2x3' );
	var M = 2;
	var N = 3;
	var LDA = 4;

	var Xdata = [ 10, 20, 30, 40, 50, 60 ];
	var X = loadMatrix( Xdata, M, N, LDA );

	// K = [1, 2, 3] -> [0, 1, 2] (0-based)
	var K = new Int32Array([ 0, 1, 2 ]);

	dlapmt( true, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X' );
});

test( 'dlapmt: N=1 quick return', function t() {
	var M = 3;
	var N = 1;
	var LDA = 4;

	var X = loadMatrix( [ 42, 43, 44 ], M, N, LDA );
	var K = new Int32Array([ 0 ]);

	dlapmt( true, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, [ 42, 43, 44 ], 1e-14, 'X' );
});

test( 'dlapmt: N=0 quick return', function t() {
	var X = new Float64Array( 1 );
	var K = new Int32Array( 1 );

	// Should not throw or modify anything
	dlapmt( true, 3, 0, X, 1, 4, 0, K, 1, 0 );
	assert.ok( true, 'no error' );
});

test( 'dlapmt: reverse permutation forward 2x4', function t() {
	var tc = findCase( 'reverse_fwd_2x4' );
	var M = 2;
	var N = 4;
	var LDA = 4;

	var Xdata = [ 1, 2, 3, 4, 5, 6, 7, 8 ];
	var X = loadMatrix( Xdata, M, N, LDA );

	// K = [4, 3, 2, 1] -> [3, 2, 1, 0] (0-based)
	var K = new Int32Array([ 3, 2, 1, 0 ]);

	dlapmt( true, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X' );
});

test( 'dlapmt: reverse permutation backward 2x4', function t() {
	var tc = findCase( 'reverse_bwd_2x4' );
	var M = 2;
	var N = 4;
	var LDA = 4;

	var Xdata = [ 1, 2, 3, 4, 5, 6, 7, 8 ];
	var X = loadMatrix( Xdata, M, N, LDA );

	// K = [4, 3, 2, 1] -> [3, 2, 1, 0] (0-based)
	var K = new Int32Array([ 3, 2, 1, 0 ]);

	dlapmt( false, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X' );
});

test( 'dlapmt: cyclic permutation forward 2x5', function t() {
	var tc = findCase( 'cyclic_fwd_2x5' );
	var M = 2;
	var N = 5;
	var LDA = 4;

	var Xdata = [ 10, 11, 20, 21, 30, 31, 40, 41, 50, 51 ];
	var X = loadMatrix( Xdata, M, N, LDA );

	// K = [2, 3, 4, 5, 1] -> [1, 2, 3, 4, 0] (0-based)
	var K = new Int32Array([ 1, 2, 3, 4, 0 ]);

	dlapmt( true, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X' );
});

test( 'dlapmt: non-unit stride for X', function t() {
	var tc = findCase( 'forward_3x4' );
	var M = 3;
	var N = 4;
	var strideX1 = 2;
	var strideX2 = strideX1 * M; // 6
	var Xdata = [ 1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 14, 15 ];
	var out;
	var X;
	var K;
	var i;
	var j;

	X = new Float64Array( strideX2 * N );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			X[ i * strideX1 + j * strideX2 ] = Xdata[ j * M + i ];
		}
	}

	K = new Int32Array([ 2, 0, 3, 1 ]);
	dlapmt( true, M, N, X, strideX1, strideX2, 0, K, 1, 0 );

	out = [];
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( X[ i * strideX1 + j * strideX2 ] );
		}
	}
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X strided' );
});

test( 'dlapmt: non-zero offset', function t() {
	var tc = findCase( 'forward_3x4' );
	var M = 3;
	var N = 4;
	var LDA = 4;
	var off = 5;
	var Xdata = [ 1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 14, 15 ];
	var out;
	var X;
	var K;
	var i;
	var j;

	X = new Float64Array( off + LDA * N );
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			X[ off + j * LDA + i ] = Xdata[ j * M + i ];
		}
	}

	K = new Int32Array([ 0, 2, 0, 3, 1 ]); // offset by 1
	dlapmt( true, M, N, X, 1, LDA, off, K, 1, 1 );

	out = [];
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( X[ off + j * LDA + i ] );
		}
	}
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X with offset' );
});

test( 'dlapmt: K is restored after forward permutation', function t() {
	var M = 2;
	var N = 4;
	var LDA = 2;
	var K;
	var X;

	X = new Float64Array([ 1, 2, 3, 4, 5, 6, 7, 8 ]);
	K = new Int32Array([ 2, 0, 3, 1 ]);

	dlapmt( true, M, N, X, 1, LDA, 0, K, 1, 0 );

	// K should be restored to original values
	assert.deepStrictEqual( Array.from( K ), [ 2, 0, 3, 1 ], 'K restored' );
});

test( 'dlapmt: K is restored after backward permutation', function t() {
	var M = 2;
	var N = 4;
	var LDA = 2;
	var K;
	var X;

	X = new Float64Array([ 1, 2, 3, 4, 5, 6, 7, 8 ]);
	K = new Int32Array([ 2, 0, 3, 1 ]);

	dlapmt( false, M, N, X, 1, LDA, 0, K, 1, 0 );

	assert.deepStrictEqual( Array.from( K ), [ 2, 0, 3, 1 ], 'K restored' );
});
