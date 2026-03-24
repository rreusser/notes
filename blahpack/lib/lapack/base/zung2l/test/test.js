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
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zung2l = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zung2l.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Load a complex matrix (M x N) from interleaved re/im pairs into a Complex128Array with LDA stride.
*
* @param {Array} data - interleaved re/im pairs, column-major dense (2*M*N elements)
* @param {number} M - number of rows
* @param {number} N - number of columns
* @param {number} LDA - leading dimension (complex elements, >= M)
* @returns {Complex128Array} buffer of size LDA*N complex elements
*/
function loadComplexMatrix( data, M, N, LDA ) {
	var A = new Complex128Array( LDA * N );
	var Av = reinterpret( A, 0 );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			Av[ 2 * (j * LDA + i) ] = data[ 2 * (j * M + i) ];
			Av[ 2 * (j * LDA + i) + 1 ] = data[ 2 * (j * M + i) + 1 ];
		}
	}
	return A;
}

/**
* Extract M-by-N submatrix from Complex128Array with leading dim LDA.
*
* @returns {Array} interleaved re/im pairs in column-major order (2*M*N elements)
*/
function extractComplexMatrix( A, LDA, M, N ) {
	var Av = reinterpret( A, 0 );
	var out = [];
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			out.push( Av[ 2 * (j * LDA + i) ] );
			out.push( Av[ 2 * (j * LDA + i) + 1 ] );
		}
	}
	return out;
}


// TESTS //

test( 'zung2l: 3x3 full Q (M=N=K=3)', function t() {
	var tc = findCase( 'zung2l_3x3' );
	var WORK = new Complex128Array( 10 );
	var LDA = 4;
	var info;
	var out;
	var TAU;
	var A;

	// Reflectors from the Fortran test (column-major, M=3, LDA=4):
	// Col 0: (0.5,0.5), (1,0), (0,0)
	// Col 1: (0,1), (0.5,-0.5), (0,0)
	// Col 2: (1,0), (0,0.5), (0.3,0)
	var Adata = new Float64Array([
		0.5, 0.5, 1, 0, 0, 0,
		0, 1, 0.5, -0.5, 0, 0,
		1, 0, 0, 0.5, 0.3, 0
	]);
	A = loadComplexMatrix( Adata, 3, 3, LDA );

	TAU = new Complex128Array( 3 );
	var tauv = reinterpret( TAU, 0 );
	tauv[ 0 ] = 1.2; tauv[ 1 ] = 0.1;
	tauv[ 2 ] = 0.8; tauv[ 3 ] = -0.2;
	tauv[ 4 ] = 1.5; tauv[ 5 ] = 0.3;

	info = zung2l( 3, 3, 3, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	// Fixture uses LDA=4, so Q has 2*LDA*N = 24 values including padding rows.
	// Extract only the 3x3 part (2*3*3 = 18 values).
	out = extractComplexMatrix( A, LDA, 3, 3 );
	// The fixture has 2*LDA*N elements; extract the dense 2*M*N subset for comparison
	var expected = [];
	var i;
	var j;
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			expected.push( tc.Q[ 2 * (j * 4 + i) ] );
			expected.push( tc.Q[ 2 * (j * 4 + i) + 1 ] );
		}
	}
	assertArrayClose( out, expected, 1e-14, 'Q' );
});

test( 'zung2l: 4x3 rectangular (M=4, N=3, K=2)', function t() {
	var tc = findCase( 'zung2l_4x3' );
	var WORK = new Complex128Array( 10 );
	var LDA = 4;
	var info;
	var out;
	var TAU;
	var A;

	// From Fortran test:
	// Col 0: (0,0), (0,0), (0,0), (0,0)
	// Col 1: (0.3,0.4), (0.5,-0.1), (1,0), (0,0)
	// Col 2: (0.2,0.1), (0.4,-0.3), (0,0.6), (0.7,0)
	var Adata = new Float64Array([
		0, 0, 0, 0, 0, 0, 0, 0,
		0.3, 0.4, 0.5, -0.1, 1, 0, 0, 0,
		0.2, 0.1, 0.4, -0.3, 0, 0.6, 0.7, 0
	]);
	A = loadComplexMatrix( Adata, 4, 3, LDA );

	TAU = new Complex128Array( 2 );
	var tauv = reinterpret( TAU, 0 );
	tauv[ 0 ] = 1.1; tauv[ 1 ] = 0.2;
	tauv[ 2 ] = 0.9; tauv[ 3 ] = -0.1;

	info = zung2l( 4, 3, 2, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	out = extractComplexMatrix( A, LDA, 4, 3 );
	// Fixture has LDA=4=M, so 2*LDA*N = 2*4*3 = 24 = 2*M*N
	assertArrayClose( out, tc.Q, 1e-14, 'Q' );
});

test( 'zung2l: K=0 (identity)', function t() {
	var tc = findCase( 'zung2l_K0' );
	var WORK = new Complex128Array( 10 );
	var TAU = new Complex128Array( 1 );
	var LDA = 4;
	var info;
	var out;
	var A;

	// K=0: should produce identity columns, input doesn't matter
	A = new Complex128Array( LDA * 3 );
	var Av = reinterpret( A, 0 );
	// Fill with arbitrary values
	Av[ 0 ] = 9; Av[ 1 ] = 9;

	info = zung2l( 3, 3, 0, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	out = extractComplexMatrix( A, LDA, 3, 3 );
	// Expected: identity matrix
	var expected = [
		1, 0, 0, 0, 0, 0,
		0, 0, 1, 0, 0, 0,
		0, 0, 0, 0, 1, 0
	];
	assertArrayClose( out, expected, 1e-14, 'Q' );
});

test( 'zung2l: N=0 quick return', function t() {
	var WORK = new Complex128Array( 1 );
	var TAU = new Complex128Array( 1 );
	var A = new Complex128Array( 1 );
	var info;

	info = zung2l( 3, 0, 0, A, 1, 4, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'zung2l: M=0, N=0 quick return', function t() {
	var WORK = new Complex128Array( 1 );
	var TAU = new Complex128Array( 1 );
	var A = new Complex128Array( 1 );
	var info;

	info = zung2l( 0, 0, 0, A, 1, 1, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'zung2l: 3x3 with non-unit LDA', function t() {
	var tc = findCase( 'zung2l_3x3' );
	var WORK = new Complex128Array( 10 );
	var LDA = 5; // larger than M=3
	var info;
	var out;
	var TAU;
	var A;

	var Adata = new Float64Array([
		0.5, 0.5, 1, 0, 0, 0,
		0, 1, 0.5, -0.5, 0, 0,
		1, 0, 0, 0.5, 0.3, 0
	]);
	A = loadComplexMatrix( Adata, 3, 3, LDA );

	TAU = new Complex128Array( 3 );
	var tauv = reinterpret( TAU, 0 );
	tauv[ 0 ] = 1.2; tauv[ 1 ] = 0.1;
	tauv[ 2 ] = 0.8; tauv[ 3 ] = -0.2;
	tauv[ 4 ] = 1.5; tauv[ 5 ] = 0.3;

	info = zung2l( 3, 3, 3, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	out = extractComplexMatrix( A, LDA, 3, 3 );
	// Extract expected from fixture (LDA=4)
	var expected = [];
	var i;
	var j;
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			expected.push( tc.Q[ 2 * (j * 4 + i) ] );
			expected.push( tc.Q[ 2 * (j * 4 + i) + 1 ] );
		}
	}
	assertArrayClose( out, expected, 1e-14, 'Q' );
});
