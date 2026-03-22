'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dorg2l = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );

// Expected outputs (Q matrices)
var outputLines = readFileSync( path.join( fixtureDir, 'dorg2l.jsonl' ), 'utf8' ).trim().split( '\n' );
var outputFixture = outputLines.map( function parse( line ) { return JSON.parse( line ); } );

// QL factorization inputs (A and TAU after DGEQL2)
var inputLines = readFileSync( path.join( fixtureDir, 'dorg2l_inputs.jsonl' ), 'utf8' ).trim().split( '\n' );
var inputFixture = inputLines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findOutput( name ) {
	return outputFixture.find( function find( t ) { return t.name === name; } );
}

function findInput( name ) {
	return inputFixture.find( function find( t ) { return t.name === name; } );
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
* Load fixture matrix (dense M*N column-major) into a Float64Array with LDA stride.
*
* Fixture stores M elements per column (no padding). This function
* unpacks into a buffer with row stride 1 and column stride LDA.
*
* @param {Array} data - dense column-major data (M*N elements)
* @param {number} M - number of rows
* @param {number} N - number of columns
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
* Extract M-by-N submatrix from column-major flat array with leading dim LDA.
*
* @param {Float64Array} A - flat column-major array
* @param {number} LDA - leading dimension (row stride)
* @param {number} M - number of rows
* @param {number} N - number of columns
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


// TESTS //

test( 'dorg2l: 4x3, K=3 (M > N, full K)', function t() {
	var expected = findOutput( '4x3_k3' );
	var inp = findInput( '4x3_ql' );
	var WORK = new Float64Array( 4 );
	var LDA = 6;
	var info;
	var out;
	var A;

	A = loadMatrix( inp.A, 4, 3, LDA );
	info = dorg2l( 4, 3, 3, A, 1, LDA, 0, new Float64Array( inp.TAU ), 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = extractMatrix( A, LDA, 4, 3 );
	assertArrayClose( out, expected.Q, 1e-14, 'Q' );
});

test( 'dorg2l: 3x3, K=3 (square)', function t() {
	var expected = findOutput( '3x3_k3' );
	var inp = findInput( '3x3_ql' );
	var WORK = new Float64Array( 4 );
	var LDA = 6;
	var info;
	var out;
	var A;

	A = loadMatrix( inp.A, 3, 3, LDA );
	info = dorg2l( 3, 3, 3, A, 1, LDA, 0, new Float64Array( inp.TAU ), 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = extractMatrix( A, LDA, 3, 3 );
	assertArrayClose( out, expected.Q, 1e-14, 'Q' );
});

test( 'dorg2l: 4x2, K=1 (K < N, partial reflectors)', function t() {
	var expected = findOutput( '4x2_k1' );
	var inp = findInput( '4x2_ql' );
	var WORK = new Float64Array( 4 );
	var LDA = 6;
	var info;
	var out;
	var A;

	A = loadMatrix( inp.A, 4, 2, LDA );

	// Fortran test passes the raw TAU array from DGEQL2; with K=1, DORG2L uses TAU(1)
	info = dorg2l( 4, 2, 1, A, 1, LDA, 0, new Float64Array( inp.TAU ), 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = extractMatrix( A, LDA, 4, 2 );
	assertArrayClose( out, expected.Q, 1e-14, 'Q' );
});

test( 'dorg2l: K=0 (identity columns)', function t() {
	var expected = findOutput( 'k_zero' );
	var WORK = new Float64Array( 4 );
	var TAU = new Float64Array( 1 );
	var LDA = 6;
	var info;
	var out;
	var A;

	// K=0 means all columns are identity columns; input A content is irrelevant
	A = new Float64Array( LDA * 2 );
	A[ 0 ] = 99.0; A[ LDA ] = 88.0;
	A[ 1 ] = 77.0; A[ LDA + 1 ] = 66.0;
	A[ 2 ] = 55.0; A[ LDA + 2 ] = 44.0;

	info = dorg2l( 3, 2, 0, A, 1, LDA, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = extractMatrix( A, LDA, 3, 2 );
	assertArrayClose( out, expected.Q, 1e-14, 'Q' );
});

test( 'dorg2l: N=0 quick return', function t() {
	var WORK = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var A = new Float64Array( 6 );
	var info;

	info = dorg2l( 3, 0, 0, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );
});

test( 'dorg2l: M=0, N=0 quick return', function t() {
	var WORK = new Float64Array( 1 );
	var TAU = new Float64Array( 1 );
	var A = new Float64Array( 1 );
	var info;

	info = dorg2l( 0, 0, 0, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );
});

test( 'dorg2l: 5x3, K=3 (verify orthogonality Q^T * Q = I)', function t() {
	var expected = findOutput( '5x3_orthogonal' );
	var inp = findInput( '5x3_ql' );
	var WORK = new Float64Array( 4 );
	var LDA = 6;
	var QtQ;
	var info;
	var out;
	var sum;
	var A;
	var i;
	var j;
	var k;
	var M = 5;
	var N = 3;

	A = loadMatrix( inp.A, M, N, LDA );
	info = dorg2l( M, N, 3, A, 1, LDA, 0, new Float64Array( inp.TAU ), 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = extractMatrix( A, LDA, M, N );
	assertArrayClose( out, expected.Q, 1e-14, 'Q' );

	// Verify Q^T * Q = I (orthogonality)
	QtQ = [];
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			sum = 0.0;
			for ( k = 0; k < M; k++ ) {
				sum += A[ i * LDA + k ] * A[ j * LDA + k ];
			}
			QtQ.push( sum );
		}
	}
	assertArrayClose( QtQ, expected.QtQ, 1e-14, 'QtQ' );
});

test( 'dorg2l: 6x4, K=4 (larger matrix)', function t() {
	var expected = findOutput( '6x4_k4' );
	var inp = findInput( '6x4_ql' );
	var WORK = new Float64Array( 4 );
	var LDA = 6;
	var info;
	var out;
	var A;

	A = loadMatrix( inp.A, 6, 4, LDA );
	info = dorg2l( 6, 4, 4, A, 1, LDA, 0, new Float64Array( inp.TAU ), 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = extractMatrix( A, LDA, 6, 4 );
	assertArrayClose( out, expected.Q, 1e-14, 'Q' );
});

test( 'dorg2l: non-unit strides', function t() {
	// Test with strideA1=2 (row stride) to verify stride handling
	var inp = findInput( '3x3_ql' );
	var expected = findOutput( '3x3_k3' );
	var WORK = new Float64Array( 4 );
	var strideA1 = 2;
	var strideA2 = strideA1 * 3; // 6, needs M*strideA1 = 3*2 = 6
	var info;
	var out;
	var A;
	var i;
	var j;

	// Dense fixture is [col0_r0..col0_r2, col1_r0..col1_r2, col2_r0..col2_r2]
	A = new Float64Array( strideA2 * 3 );
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			A[ i * strideA1 + j * strideA2 ] = inp.A[ j * 3 + i ];
		}
	}

	info = dorg2l( 3, 3, 3, A, strideA1, strideA2, 0, new Float64Array( inp.TAU ), 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	// Extract with stride
	out = [];
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			out.push( A[ i * strideA1 + j * strideA2 ] );
		}
	}
	assertArrayClose( out, expected.Q, 1e-14, 'Q strided' );
});

test( 'dorg2l: non-zero offset', function t() {
	// Test with offsetA > 0
	var inp = findInput( '3x3_ql' );
	var expected = findOutput( '3x3_k3' );
	var WORK = new Float64Array( 4 );
	var LDA = 6;
	var off = 5;
	var info;
	var out;
	var A;
	var i;
	var j;

	// Allocate with extra space at the beginning
	A = new Float64Array( off + LDA * 3 );
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			A[ off + j * LDA + i ] = inp.A[ j * 3 + i ];
		}
	}

	var TAU_arr = new Float64Array( 1 + 3 );
	TAU_arr[ 1 ] = inp.TAU[ 0 ];
	TAU_arr[ 2 ] = inp.TAU[ 1 ];
	TAU_arr[ 3 ] = inp.TAU[ 2 ];

	info = dorg2l( 3, 3, 3, A, 1, LDA, off, TAU_arr, 1, 1, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = [];
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			out.push( A[ off + j * LDA + i ] );
		}
	}
	assertArrayClose( out, expected.Q, 1e-14, 'Q with offset' );
});
