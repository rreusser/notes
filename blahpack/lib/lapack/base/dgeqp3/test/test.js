'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgeqp3 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgeqp3.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// VARIABLES //

var LDA = 8; // Matches Fortran MAXMN


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
* Creates a column-major matrix from values.
* vals is an array of column-major values for an M-by-N matrix, stored with leading dimension M.
* Returns a Float64Array of size LDA*N with values placed using leading dimension LDA.
*/
function makeMatrix( vals, M, N ) {
	var A = new Float64Array( LDA * N );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			A[ j * LDA + i ] = vals[ j * M + i ];
		}
	}
	return A;
}

/**
* Extracts column-major values from matrix A (LDA-by-N) as M-by-N.
*/
function extractMatrix( A, M, N ) {
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

test( 'dgeqp3: 4x3 matrix (tall)', function t() {
	var tc = findCase( 'rect_4x3' );
	// A = [1 0 3; 2 1 0; 0 3 1; 1 2 2] column-major
	var A = makeMatrix( [1, 2, 0, 1, 0, 1, 3, 2, 3, 0, 1, 2], 4, 3 );
	var JPVT = new Int32Array( 3 ); // all zeros = free
	var TAU = new Float64Array( 3 );

	var info = dgeqp3( 4, 3, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( extractMatrix( A, 4, 3 ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'dgeqp3: 3x4 matrix (wide)', function t() {
	var tc = findCase( 'rect_3x4' );
	// A = [1 3 0 1; 0 1 2 0; 2 0 1 3] column-major
	var A = makeMatrix( [1, 0, 2, 3, 1, 0, 0, 2, 1, 1, 0, 3], 3, 4 );
	var JPVT = new Int32Array( 4 );
	var TAU = new Float64Array( 3 );

	var info = dgeqp3( 3, 4, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( extractMatrix( A, 3, 4 ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'dgeqp3: square 4x4', function t() {
	var tc = findCase( 'square_4x4' );
	// A = [2 0 1 3; 1 3 0 2; 0 1 4 1; 1 2 1 5] column-major
	var A = makeMatrix( [2, 1, 0, 1, 0, 3, 1, 2, 1, 0, 4, 1, 3, 2, 1, 5], 4, 4 );
	var JPVT = new Int32Array( 4 );
	var TAU = new Float64Array( 4 );

	var info = dgeqp3( 4, 4, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( extractMatrix( A, 4, 4 ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'dgeqp3: N=0 (quick return)', function t() {
	var A = new Float64Array( 1 );
	var JPVT = new Int32Array( 1 );
	var TAU = new Float64Array( 1 );

	var info = dgeqp3( 3, 0, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0 );

	assert.equal( info, 0, 'info' );
});

test( 'dgeqp3: M=0 (quick return)', function t() {
	var A = new Float64Array( 1 );
	var JPVT = new Int32Array( 3 );
	var TAU = new Float64Array( 1 );

	var info = dgeqp3( 0, 3, A, 1, 1, 0, JPVT, 1, 0, TAU, 1, 0 );

	assert.equal( info, 0, 'info' );
});

test( 'dgeqp3: 1x1 matrix', function t() {
	var tc = findCase( 'one_by_one' );
	var A = new Float64Array( LDA );
	A[ 0 ] = 5.0;
	var JPVT = new Int32Array( 1 );
	var TAU = new Float64Array( 1 );

	var info = dgeqp3( 1, 1, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( extractMatrix( A, 1, 1 ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'dgeqp3: fixed column 1', function t() {
	var tc = findCase( 'fixed_col1' );
	// A = [1 0 0; 0 3 1; 0 4 2] column-major
	var A = makeMatrix( [1, 0, 0, 0, 3, 4, 0, 1, 2], 3, 3 );
	var JPVT = new Int32Array( [1, 0, 0] ); // fix column 1
	var TAU = new Float64Array( 3 );

	var info = dgeqp3( 3, 3, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( extractMatrix( A, 3, 3 ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'dgeqp3: fixed column 3 (swap to front)', function t() {
	var tc = findCase( 'fixed_col3_swap' );
	// A = [1 3 0; 2 0 1; 0 2 3; 1 1 2] column-major
	var A = makeMatrix( [1, 2, 0, 1, 3, 0, 2, 1, 0, 1, 3, 2], 4, 3 );
	var JPVT = new Int32Array( [0, 0, 1] ); // fix column 3
	var TAU = new Float64Array( 3 );

	var info = dgeqp3( 4, 3, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( extractMatrix( A, 4, 3 ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'dgeqp3: fix columns 1 and 3', function t() {
	var tc = findCase( 'fixed_two_cols' );
	// Same matrix as fixed_col3_swap test
	var A = makeMatrix( [1, 2, 0, 1, 3, 0, 2, 1, 0, 1, 3, 2], 4, 3 );
	var JPVT = new Int32Array( [1, 0, 1] ); // fix columns 1 and 3
	var TAU = new Float64Array( 3 );

	var info = dgeqp3( 4, 3, A, 1, LDA, 0, JPVT, 1, 0, TAU, 1, 0 );

	assert.equal( info, tc.info, 'info' );
	assertArrayClose( extractMatrix( A, 4, 3 ), tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'dgeqp3: wide 8x36 (unblocked, sminmn < NB)', function t() {
	var tc = findCase( 'wide_8x36' );
	var BIGMN = 40;
	var M = 8;
	var N = 36;
	var A = new Float64Array( BIGMN * N );
	var i;
	var j;

	// Fill matrix: AB(i,j) = mod(i*j + 3*i + 7, 11) - 5.0
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			A[ j * BIGMN + i ] = ((i + 1) * (j + 1) + 3 * (i + 1) + 7) % 11 - 5.0;
		}
	}

	var JPVT = new Int32Array( N );
	var TAU = new Float64Array( M );

	var info = dgeqp3( M, N, A, 1, BIGMN, 0, JPVT, 1, 0, TAU, 1, 0 );

	assert.equal( info, tc.info, 'info' );

	// Extract M-by-N from BIGMN-by-N storage
	var aOut = [];
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			aOut.push( A[ j * BIGMN + i ] );
		}
	}
	assertArrayClose( aOut, tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});

test( 'dgeqp3: large 140x130 (triggers blocked dlaqps path)', function t() {
	var tc = findCase( 'large_140x130_blocked' );
	var BIGMN = 140;
	var M = 140;
	var N = 130;
	var A = new Float64Array( BIGMN * N );
	var i;
	var j;

	// Fill matrix: AB(i,j) = sin(i*0.7 + j*1.3) + cos(i*j*0.3)
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			A[ j * BIGMN + i ] = Math.sin( (i + 1) * 0.7 + (j + 1) * 1.3 ) + Math.cos( (i + 1) * (j + 1) * 0.3 );
		}
	}

	var JPVT = new Int32Array( N );
	var TAU = new Float64Array( N );

	var info = dgeqp3( M, N, A, 1, BIGMN, 0, JPVT, 1, 0, TAU, 1, 0 );

	assert.equal( info, tc.info, 'info' );

	// Extract M-by-N from BIGMN-by-N storage
	var aOut = [];
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			aOut.push( A[ j * BIGMN + i ] );
		}
	}
	assertArrayClose( aOut, tc.a, 1e-10, 'a' );
	assertArrayClose( Array.from( TAU ), tc.tau, 1e-10, 'tau' );
	assert.deepStrictEqual( Array.from( JPVT ), tc.jpvt, 'jpvt' );
});
