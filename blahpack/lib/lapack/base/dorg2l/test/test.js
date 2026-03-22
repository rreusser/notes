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
* Extract M-by-N submatrix from column-major flat array with leading dim LDA.
*
* @param {Float64Array} A - flat column-major array
* @param {number} LDA - leading dimension (row stride)
* @param {number} M - number of rows
* @param {number} N - number of columns
* @returns {Array} extracted values in column-major order
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
	var info;
	var out;
	var A;

	// Copy QL factorization result into 6-row leading dimension array (col-major)
	A = new Float64Array( 6 * 3 );
	A.set( inp.A );

	info = dorg2l( 4, 3, 3, A, 1, 6, 0, new Float64Array( inp.TAU ), 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = extractMatrix( A, 6, 4, 3 );
	assertArrayClose( out, expected.Q, 1e-14, 'Q' );
});

test( 'dorg2l: 3x3, K=3 (square)', function t() {
	var expected = findOutput( '3x3_k3' );
	var inp = findInput( '3x3_ql' );
	var WORK = new Float64Array( 4 );
	var info;
	var out;
	var A;

	A = new Float64Array( 6 * 3 );
	A.set( inp.A );

	info = dorg2l( 3, 3, 3, A, 1, 6, 0, new Float64Array( inp.TAU ), 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = extractMatrix( A, 6, 3, 3 );
	assertArrayClose( out, expected.Q, 1e-14, 'Q' );
});

test( 'dorg2l: 4x2, K=1 (K < N, partial reflectors)', function t() {
	var expected = findOutput( '4x2_k1' );
	var inp = findInput( '4x2_ql' );
	var WORK = new Float64Array( 4 );
	var info;
	var out;
	var A;

	A = new Float64Array( 6 * 2 );
	A.set( inp.A );

	info = dorg2l( 4, 2, 1, A, 1, 6, 0, new Float64Array( [ inp.TAU[1] ] ), 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = extractMatrix( A, 6, 4, 2 );
	assertArrayClose( out, expected.Q, 1e-14, 'Q' );
});

test( 'dorg2l: K=0 (identity columns)', function t() {
	var expected = findOutput( 'k_zero' );
	var WORK = new Float64Array( 4 );
	var TAU = new Float64Array( 1 );
	var info;
	var out;
	var A;

	// K=0 means all columns are identity columns; input A content is irrelevant
	A = new Float64Array( 6 * 2 );
	A[ 0 ] = 99.0; A[ 6 ] = 88.0;
	A[ 1 ] = 77.0; A[ 7 ] = 66.0;
	A[ 2 ] = 55.0; A[ 8 ] = 44.0;

	info = dorg2l( 3, 2, 0, A, 1, 6, 0, TAU, 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = extractMatrix( A, 6, 3, 2 );
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
	var QtQ;
	var info;
	var out;
	var A;
	var i;
	var j;
	var k;
	var M = 5;
	var N = 3;

	A = new Float64Array( 6 * 3 );
	A.set( inp.A );

	info = dorg2l( M, N, 3, A, 1, 6, 0, new Float64Array( inp.TAU ), 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = extractMatrix( A, 6, M, N );
	assertArrayClose( out, expected.Q, 1e-14, 'Q' );

	// Verify Q^T * Q = I (orthogonality)
	QtQ = [];
	for ( i = 0; i < N; i++ ) {
		for ( j = 0; j < N; j++ ) {
			var sum = 0.0;
			for ( k = 0; k < M; k++ ) {
				sum += A[ i * 6 + k ] * A[ j * 6 + k ];
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
	var info;
	var out;
	var A;

	A = new Float64Array( 6 * 4 );
	A.set( inp.A );

	info = dorg2l( 6, 4, 4, A, 1, 6, 0, new Float64Array( inp.TAU ), 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = extractMatrix( A, 6, 6, 4 );
	assertArrayClose( out, expected.Q, 1e-14, 'Q' );
});

test( 'dorg2l: non-unit strides', function t() {
	// Test with strideA1=2 (row stride) to verify stride handling
	var inp = findInput( '3x3_ql' );
	var expected = findOutput( '3x3_k3' );
	var WORK = new Float64Array( 4 );
	var info;
	var out;
	var A;
	var i;
	var j;

	// Pack with row stride = 2, col stride = 2*3 = 6
	// A has 3 rows, 3 cols => need 2*3 * 3 = 18 elements with stride
	A = new Float64Array( 6 * 3 );
	// Copy from inp.A (col-major, LDA=6) into strided layout (strideA1=2, strideA2=6)
	// inp.A is [col0_row0, col0_row1, col0_row2, ..., col0_row5, col1_row0, ...]
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			A[ i * 2 + j * 6 ] = inp.A[ j * 6 + i ];
		}
	}

	info = dorg2l( 3, 3, 3, A, 2, 6, 0, new Float64Array( inp.TAU ), 1, 0, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	// Extract with stride
	out = [];
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			out.push( A[ i * 2 + j * 6 ] );
		}
	}
	assertArrayClose( out, expected.Q, 1e-14, 'Q strided' );
});

test( 'dorg2l: non-zero offset', function t() {
	// Test with offsetA > 0
	var inp = findInput( '3x3_ql' );
	var expected = findOutput( '3x3_k3' );
	var WORK = new Float64Array( 4 );
	var off = 5;
	var info;
	var out;
	var A;
	var i;
	var j;

	// Allocate with extra space at the beginning
	A = new Float64Array( off + 6 * 3 );
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			A[ off + j * 6 + i ] = inp.A[ j * 6 + i ];
		}
	}

	var TAU_arr = new Float64Array( 1 + 3 );
	TAU_arr[ 1 ] = inp.TAU[ 0 ];
	TAU_arr[ 2 ] = inp.TAU[ 1 ];
	TAU_arr[ 3 ] = inp.TAU[ 2 ];

	info = dorg2l( 3, 3, 3, A, 1, 6, off, TAU_arr, 1, 1, WORK, 1, 0 );
	assert.equal( info, 0, 'INFO' );

	out = [];
	for ( j = 0; j < 3; j++ ) {
		for ( i = 0; i < 3; i++ ) {
			out.push( A[ off + j * 6 + i ] );
		}
	}
	assertArrayClose( out, expected.Q, 1e-14, 'Q with offset' );
});
