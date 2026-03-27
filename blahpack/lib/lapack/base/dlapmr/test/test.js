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
var dlapmr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlapmr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );

// MMAX in Fortran test (leading dimension of X)
var MMAX = 5;


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var relErr;
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i += 1 ) {
		relErr = Math.abs( actual[ i ] - expected[ i ] ) / Math.max( Math.abs( expected[ i ] ), 1.0 );
		assert.ok( relErr <= tol, msg + '[' + i + ']: expected ' + expected[ i ] + ', got ' + actual[ i ] );
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
	for ( j = 0; j < N; j += 1 ) {
		for ( i = 0; i < M; i += 1 ) {
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
	for ( j = 0; j < N; j += 1 ) {
		for ( i = 0; i < M; i += 1 ) {
			out.push( A[ j * LDA + i ] );
		}
	}
	return out;
}

/**
* Extract M-by-N submatrix from fixture's X (which has LDA=MMAX=5).
*/
function extractFixtureX( tc ) {
	return extractMatrix( tc.X, MMAX, tc.M, tc.N );
}


// TESTS //

test( 'dlapmr is a function', function t() {
	assert.equal( typeof dlapmr, 'function' );
});

test( 'dlapmr: forward permutation 4x3', function t() {
	var tc = findCase( 'forward_4x3' );
	var M = 4;
	var N = 3;
	var LDA = M;

	// Input matrix (column-major, M=4, N=3):
	// row 0: [1, 5, 9], row 1: [2, 6, 10], row 2: [3, 7, 11], row 3: [4, 8, 12]
	var Xdata = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ];
	var X = loadMatrix( Xdata, M, N, LDA );

	// K = [3, 1, 4, 2] in Fortran (1-based) -> [2, 0, 3, 1] in JS (0-based)
	var K = new Int32Array([ 2, 0, 3, 1 ]);

	dlapmr( true, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X' );
});

test( 'dlapmr: backward permutation 4x3', function t() {
	var tc = findCase( 'backward_4x3' );
	var M = 4;
	var N = 3;
	var LDA = M;

	var Xdata = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ];
	var X = loadMatrix( Xdata, M, N, LDA );

	// K = [3, 1, 4, 2] -> [2, 0, 3, 1] (0-based)
	var K = new Int32Array([ 2, 0, 3, 1 ]);

	dlapmr( false, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X' );
});

test( 'dlapmr: identity permutation 3x2', function t() {
	var tc = findCase( 'identity_3x2' );
	var M = 3;
	var N = 2;
	var LDA = M;

	var Xdata = [ 10, 20, 30, 40, 50, 60 ];
	var X = loadMatrix( Xdata, M, N, LDA );

	// K = [1, 2, 3] -> [0, 1, 2] (0-based)
	var K = new Int32Array([ 0, 1, 2 ]);

	dlapmr( true, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X' );
});

test( 'dlapmr: M=1 quick return', function t() {
	var M = 1;
	var N = 3;
	var LDA = M;

	var X = loadMatrix( [ 42, 43, 44 ], M, N, LDA );
	var K = new Int32Array([ 0 ]);

	dlapmr( true, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, [ 42, 43, 44 ], 1e-14, 'X' );
});

test( 'dlapmr: M=0 quick return', function t() {
	var X = new Float64Array( 1 );
	var K = new Int32Array( 1 );

	// Should not throw or modify anything
	dlapmr( true, 0, 3, X, 1, 1, 0, K, 1, 0 );
	assert.ok( true, 'no error' );
});

test( 'dlapmr: reverse permutation forward 4x2', function t() {
	var tc = findCase( 'reverse_fwd_4x2' );
	var M = 4;
	var N = 2;
	var LDA = M;

	var Xdata = [ 1, 2, 3, 4, 5, 6, 7, 8 ];
	var X = loadMatrix( Xdata, M, N, LDA );

	// K = [4, 3, 2, 1] -> [3, 2, 1, 0] (0-based)
	var K = new Int32Array([ 3, 2, 1, 0 ]);

	dlapmr( true, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X' );
});

test( 'dlapmr: reverse permutation backward 4x2', function t() {
	var tc = findCase( 'reverse_bwd_4x2' );
	var M = 4;
	var N = 2;
	var LDA = M;

	var Xdata = [ 1, 2, 3, 4, 5, 6, 7, 8 ];
	var X = loadMatrix( Xdata, M, N, LDA );

	// K = [4, 3, 2, 1] -> [3, 2, 1, 0] (0-based)
	var K = new Int32Array([ 3, 2, 1, 0 ]);

	dlapmr( false, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X' );
});

test( 'dlapmr: cyclic permutation forward 5x2', function t() {
	var tc = findCase( 'cyclic_fwd_5x2' );
	var M = 5;
	var N = 2;
	var LDA = M;

	var Xdata = [ 10, 20, 30, 40, 50, 11, 21, 31, 41, 51 ];
	var X = loadMatrix( Xdata, M, N, LDA );

	// K = [2, 3, 4, 5, 1] -> [1, 2, 3, 4, 0] (0-based)
	var K = new Int32Array([ 1, 2, 3, 4, 0 ]);

	dlapmr( true, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X' );
});

test( 'dlapmr: cyclic permutation backward 5x2', function t() {
	var tc = findCase( 'cyclic_bwd_5x2' );
	var M = 5;
	var N = 2;
	var LDA = M;

	var Xdata = [ 10, 20, 30, 40, 50, 11, 21, 31, 41, 51 ];
	var X = loadMatrix( Xdata, M, N, LDA );

	// K = [2, 3, 4, 5, 1] -> [1, 2, 3, 4, 0] (0-based)
	var K = new Int32Array([ 1, 2, 3, 4, 0 ]);

	dlapmr( false, M, N, X, 1, LDA, 0, K, 1, 0 );

	var out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X' );
});

test( 'dlapmr: non-unit stride for X', function t() {
	var tc = findCase( 'forward_4x3' );
	var strideX2 = 8;
	var strideX1 = 2;
	var Xdata = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ];
	var M = 4;
	var N = 3;
	var out;
	var X;
	var K;
	var i;
	var j;

	X = new Float64Array( strideX2 * N );
	for ( j = 0; j < N; j += 1 ) {
		for ( i = 0; i < M; i += 1 ) {
			X[ i * strideX1 + j * strideX2 ] = Xdata[ j * M + i ];
		}
	}

	K = new Int32Array([ 2, 0, 3, 1 ]);
	dlapmr( true, M, N, X, strideX1, strideX2, 0, K, 1, 0 );

	out = [];
	for ( j = 0; j < N; j += 1 ) {
		for ( i = 0; i < M; i += 1 ) {
			out.push( X[ i * strideX1 + j * strideX2 ] );
		}
	}
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X strided' );
});

test( 'dlapmr: non-zero offset', function t() {
	var tc = findCase( 'forward_4x3' );
	var Xdata = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ];
	var off = 7;
	var LDA = 4;
	var M = 4;
	var N = 3;
	var out;
	var X;
	var K;
	var i;
	var j;

	X = new Float64Array( off + LDA * N );
	for ( j = 0; j < N; j += 1 ) {
		for ( i = 0; i < M; i += 1 ) {
			X[ off + j * LDA + i ] = Xdata[ j * M + i ];
		}
	}

	// K with offset 1: first element is padding
	K = new Int32Array([ 99, 2, 0, 3, 1 ]);
	dlapmr( true, M, N, X, 1, LDA, off, K, 1, 1 );

	out = [];
	for ( j = 0; j < N; j += 1 ) {
		for ( i = 0; i < M; i += 1 ) {
			out.push( X[ off + j * LDA + i ] );
		}
	}
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X with offset' );
});

test( 'dlapmr: K is restored after forward permutation', function t() {
	var LDA = 4;
	var M = 4;
	var N = 2;
	var K;
	var X;

	X = new Float64Array([ 1, 2, 3, 4, 5, 6, 7, 8 ]);
	K = new Int32Array([ 2, 0, 3, 1 ]);

	dlapmr( true, M, N, X, 1, LDA, 0, K, 1, 0 );

	// K should be restored to original values
	assert.deepStrictEqual( Array.from( K ), [ 2, 0, 3, 1 ], 'K restored' );
});

test( 'dlapmr: K is restored after backward permutation', function t() {
	var LDA = 4;
	var M = 4;
	var N = 2;
	var K;
	var X;

	X = new Float64Array([ 1, 2, 3, 4, 5, 6, 7, 8 ]);
	K = new Int32Array([ 2, 0, 3, 1 ]);

	dlapmr( false, M, N, X, 1, LDA, 0, K, 1, 0 );

	assert.deepStrictEqual( Array.from( K ), [ 2, 0, 3, 1 ], 'K restored' );
});

test( 'dlapmr: non-unit stride for K', function t() {
	var tc = findCase( 'forward_4x3' );
	var Xdata = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ];
	var LDA = 4;
	var M = 4;
	var N = 3;
	var out;
	var X;
	var K;

	X = loadMatrix( Xdata, M, N, LDA );

	// K = [2, 0, 3, 1] interleaved with padding: stride=2
	K = new Int32Array([ 2, 99, 0, 99, 3, 99, 1, 99 ]);

	dlapmr( true, M, N, X, 1, LDA, 0, K, 2, 0 );

	out = extractMatrix( X, LDA, M, N );
	assertArrayClose( out, extractFixtureX( tc ), 1e-14, 'X with strided K' );

	// K should be restored
	assert.equal( K[ 0 ], 2, 'K[0] restored' );
	assert.equal( K[ 2 ], 0, 'K[2] restored' );
	assert.equal( K[ 4 ], 3, 'K[4] restored' );
	assert.equal( K[ 6 ], 1, 'K[6] restored' );
});
