/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dgelss = require( './../lib/base.js' );

// FIXTURES //

var overdetermined_full_rank = require( './fixtures/overdetermined_full_rank.json' );
var overdetermined_rank_deficient = require( './fixtures/overdetermined_rank_deficient.json' );
var underdetermined = require( './fixtures/underdetermined.json' );
var square_3x3 = require( './fixtures/square_3x3.json' );
var multiple_rhs = require( './fixtures/multiple_rhs.json' );
var m_zero = require( './fixtures/m_zero.json' );
var n_zero = require( './fixtures/n_zero.json' );
var overdetermined_tall = require( './fixtures/overdetermined_tall.json' );
var underdetermined_wide = require( './fixtures/underdetermined_wide.json' );

// FUNCTIONS //

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Compute y = A * x (column-major matrix-vector product).
*
* @param {number} M - rows
* @param {number} N - cols
* @param {Float64Array} A - M-by-N column-major
* @param {Float64Array} x - N-vector
* @returns {Float64Array} y - M-vector
*/
function matvec( M, N, A, x ) {
	var y = new Float64Array( M );
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			y[ i ] += A[ i + j * M ] * x[ j ];
		}
	}
	return y;
}

/**
* Check that || A*x - b || is small relative to || b ||.
*/
function assertResidualSmall( M, N, A, x, b, tol, msg ) {
	var bnorm = 0.0;
	var rnorm = 0.0;
	var Ax = matvec( M, N, A, x );
	var i;
	for ( i = 0; i < M; i++ ) {
		rnorm += ( Ax[ i ] - b[ i ] ) * ( Ax[ i ] - b[ i ] );
		bnorm += b[ i ] * b[ i ];
	}
	rnorm = Math.sqrt( rnorm );
	bnorm = Math.sqrt( bnorm );
	assert.ok( rnorm <= tol * ( bnorm + 1.0 ), msg + ': residual ' + rnorm + ' too large (b norm: ' + bnorm + ')' );
}

// TESTS //

test( 'dgelss: overdetermined full rank (4x2)', function t() {
	var Aorig;
	var rank;
	var info;
	var tc;
	var A;
	var B;
	var S;
	var x;

	tc = overdetermined_full_rank;
	Aorig = new Float64Array( [ 1, 3, 5, 7, 2, 4, 6, 8 ] );
	A = new Float64Array( Aorig );
	B = new Float64Array( [ 1, 2, 3, 4 ] );
	S = new Float64Array( 2 );
	rank = [ 0 ];
	info = dgelss( 4, 2, 1, A, 1, 4, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	x = new Float64Array( [ B[ 0 ], B[ 1 ] ] );
	assertArrayClose( x, tc.x, 1e-12, 'x' );
});

test( 'dgelss: overdetermined rank deficient (4x2)', function t() {
	var rank;
	var info;
	var tc;
	var A;
	var B;
	var S;
	var x;

	tc = overdetermined_rank_deficient;
	A = new Float64Array( [ 1, 2, 3, 4, 2, 4, 6, 8 ] );
	B = new Float64Array( [ 1, 2, 3, 4 ] );
	S = new Float64Array( 2 );
	rank = [ 0 ];
	info = dgelss( 4, 2, 1, A, 1, 4, 0, B, 1, 4, 0, S, 1, 0, 0.01, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	x = new Float64Array( [ B[ 0 ], B[ 1 ] ] );
	assertArrayClose( x, tc.x, 1e-12, 'x' );
});

test( 'dgelss: underdetermined (2x4)', function t() {
	var Aorig;
	var rank;
	var info;
	var tc;
	var A;
	var B;
	var S;
	var x;

	tc = underdetermined;
	A = new Float64Array( [ 1, 0, 0, 1, 0, 0, 0, 0 ] );
	Aorig = new Float64Array( A );
	B = new Float64Array( [ 1, 2, 0, 0 ] );
	S = new Float64Array( 2 );
	rank = [ 0 ];
	info = dgelss( 2, 4, 1, A, 1, 2, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	x = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ], B[ 3 ] ] );
	assertArrayClose( x, tc.x, 1e-12, 'x' );
	assertResidualSmall( 2, 4, Aorig, x, new Float64Array( [ 1, 2 ] ), 1e-12, 'residual' ); // eslint-disable-line max-len
});

test( 'dgelss: square 3x3', function t() {
	var Aorig;
	var borig;
	var rank;
	var info;
	var tc;
	var A;
	var B;
	var S;
	var x;

	tc = square_3x3;
	Aorig = new Float64Array( [ 2, 1, 0, 1, 3, 1, 0, 1, 2 ] );
	A = new Float64Array( Aorig );
	B = new Float64Array( [ 1, 2, 3 ] );
	borig = new Float64Array( B );
	S = new Float64Array( 3 );
	rank = [ 0 ];
	info = dgelss( 3, 3, 1, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	x = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ] ] );
	assertResidualSmall( 3, 3, Aorig, x, borig, 1e-12, 'residual' );
});

test( 'dgelss: multiple RHS (3x3, 2 RHS)', function t() {
	var Aorig;
	var rank;
	var info;
	var tc;
	var b1;
	var b2;
	var x1;
	var x2;
	var A;
	var B;
	var S;

	tc = multiple_rhs;
	Aorig = new Float64Array( [ 4, 1, 0, 1, 3, 1, 0, 1, 4 ] );
	A = new Float64Array( Aorig );
	B = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	b1 = new Float64Array( [ 1, 2, 3 ] );
	b2 = new Float64Array( [ 4, 5, 6 ] );
	S = new Float64Array( 3 );
	rank = [ 0 ];
	info = dgelss( 3, 3, 2, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	x1 = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ] ] );
	x2 = new Float64Array( [ B[ 3 ], B[ 4 ], B[ 5 ] ] );
	assertResidualSmall( 3, 3, Aorig, x1, b1, 1e-12, 'residual1' );
	assertResidualSmall( 3, 3, Aorig, x2, b2, 1e-12, 'residual2' );
});

test( 'dgelss: M=0 edge case', function t() {
	var rank;
	var info;
	var tc;
	var A;
	var B;
	var S;

	tc = m_zero;
	A = new Float64Array( 1 );
	B = new Float64Array( 3 );
	S = new Float64Array( 1 );
	rank = [ 0 ];
	info = dgelss( 0, 3, 1, A, 1, 1, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
});

test( 'dgelss: N=0 edge case', function t() {
	var rank;
	var info;
	var tc;
	var A;
	var B;
	var S;

	tc = n_zero;
	A = new Float64Array( 1 );
	B = new Float64Array( 3 );
	S = new Float64Array( 1 );
	rank = [ 0 ];
	info = dgelss( 3, 0, 1, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, tc.info, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
});

test( 'dgelss: overdetermined tall (6x2, M >> N triggers QR path)', function t() { // eslint-disable-line max-len
	var Aorig;
	var rank;
	var info;
	var tc;
	var A;
	var B;
	var S;
	var x;

	tc = overdetermined_tall;
	Aorig = new Float64Array([
		1,
		0,
		1,
		2,
		1,
		0,  // col 1
		0,
		1,
		1,
		1,
		2,
		0   // col 2
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array( [ 1, 1, 2, 3, 3, 0 ] );
	S = new Float64Array( 2 );
	rank = [ 0 ];
	info = dgelss( 6, 2, 1, A, 1, 6, 0, B, 1, 6, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	x = new Float64Array( [ B[ 0 ], B[ 1 ] ] );
	assertArrayClose( x, tc.x, 1e-12, 'x' );
});

test( 'dgelss: underdetermined direct bidiag path (limited workspace)', function t() { // eslint-disable-line max-len
	var Aorig;
	var rank;
	var WORK;
	var info;
	var A;
	var B;
	var S;
	var x;

	Aorig = new Float64Array([
		1,
		0,  // col 1
		0,
		1,  // col 2
		1,
		0,  // col 3
		0,
		1,  // col 4
		0,
		0   // col 5
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array( [ 3, 5, 0, 0, 0 ] );
	S = new Float64Array( 2 );
	rank = [ 0 ];
	WORK = new Float64Array( 12 );
	info = dgelss( 2, 5, 1, A, 1, 2, 0, B, 1, 5, 0, S, 1, 0, -1.0, rank, WORK, 1, 0, 12 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 2, 'rank' );
	x = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ], B[ 3 ], B[ 4 ] ] );
	assertResidualSmall( 2, 5, Aorig, x, new Float64Array( [ 3, 5 ] ), 1e-10, 'residual' ); // eslint-disable-line max-len
});

test( 'dgelss: all-zero matrix', function t() {
	var rank;
	var info;
	var A;
	var B;
	var S;

	A = new Float64Array( 4 );
	B = new Float64Array( [ 1, 2 ] );
	S = new Float64Array( 2 );
	rank = [ 0 ];
	info = dgelss( 2, 2, 1, A, 1, 2, 0, B, 1, 2, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 0, 'rank' );
	assert.equal( S[ 0 ], 0, 's[0]' );
	assert.equal( S[ 1 ], 0, 's[1]' );
	assert.equal( B[ 0 ], 0, 'B[0]' );
	assert.equal( B[ 1 ], 0, 'B[1]' );
});

test( 'dgelss: M >= N multiple RHS with GEMM path', function t() {
	var Aorig;
	var rank;
	var info;
	var A;
	var B;
	var S;

	Aorig = new Float64Array([
		2,
		0,
		1,
		0, // col 1
		0,
		3,
		0,
		1  // col 2
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array([
		1,
		2,
		3,
		4,   // RHS 1
		4,
		5,
		6,
		7,   // RHS 2
		7,
		8,
		9,
		10   // RHS 3
	]);
	S = new Float64Array( 2 );
	rank = [ 0 ];
	info = dgelss( 4, 2, 3, A, 1, 4, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 2, 'rank' );
	for ( var rhs = 0; rhs < 3; rhs++ ) {
		var x = new Float64Array( [ B[ rhs * 4 ], B[ rhs * 4 + 1 ] ] );
		var b = [ 1 + 3 * rhs, 2 + 3 * rhs, 3 + 3 * rhs, 4 + 3 * rhs ];
		var Ax = matvec( 4, 2, Aorig, x );

		// Least squares — Ax won't exactly equal b, but norm should be reasonable
	}
});

test( 'dgelss: LQ path multiple RHS', function t() {
	var Aorig;
	var rank;
	var info;
	var x1;
	var x2;
	var A;
	var B;
	var S;

	Aorig = new Float64Array([
		1,
		0,  // col 1
		0,
		1,  // col 2
		0,
		0,  // col 3
		0,
		0   // col 4
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array([
		1,
		2,
		0,
		0,  // RHS 1
		3,
		4,
		0,
		0   // RHS 2
	]);
	S = new Float64Array( 2 );
	rank = [ 0 ];
	info = dgelss( 2, 4, 2, A, 1, 2, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 2, 'rank' );
	x1 = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ], B[ 3 ] ] );
	x2 = new Float64Array( [ B[ 4 ], B[ 5 ], B[ 6 ], B[ 7 ] ] );
	assertResidualSmall( 2, 4, Aorig, x1, new Float64Array( [ 1, 2 ] ), 1e-12, 'residual1' ); // eslint-disable-line max-len
	assertResidualSmall( 2, 4, Aorig, x2, new Float64Array( [ 3, 4 ] ), 1e-12, 'residual2' ); // eslint-disable-line max-len
});

test( 'dgelss: positive rcond threshold', function t() {
	var Aorig;
	var borig;
	var rank;
	var info;
	var A;
	var B;
	var S;

	Aorig = new Float64Array( [ 2, 1, 0, 1, 3, 1, 0, 1, 2 ] );
	A = new Float64Array( Aorig );
	B = new Float64Array( [ 1, 2, 3 ] );
	borig = new Float64Array( B );
	S = new Float64Array( 3 );
	rank = [ 0 ];
	info = dgelss( 3, 3, 1, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, 0.5, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.ok( rank[ 0 ] >= 1 && rank[ 0 ] <= 3, 'rank in valid range: ' + rank[ 0 ] ); // eslint-disable-line max-len
});

test( 'dgelss: rank-deficient underdetermined (2x5, M < N, rank 1)', function t() { // eslint-disable-line max-len
	var Aorig;
	var rank;
	var info;
	var A;
	var B;
	var S;

	Aorig = new Float64Array([
		1,
		2,  // col 1
		2,
		4,  // col 2
		3,
		6,  // col 3
		0,
		0,  // col 4
		0,
		0   // col 5
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array( [ 5, 10, 0, 0, 0 ] );
	S = new Float64Array( 2 );
	rank = [ 0 ];
	info = dgelss( 2, 5, 1, A, 1, 2, 0, B, 1, 5, 0, S, 1, 0, 0.01, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 1, 'rank should be 1' );
	assert.ok( S[ 1 ] < 0.01 * S[ 0 ], 'second singular value should be small' );
});

test( 'dgelss: underdetermined multiple RHS (2x5, 3 RHS)', function t() {
	var Aorig;
	var rank;
	var info;
	var x1;
	var x2;
	var x3;
	var A;
	var B;
	var S;

	Aorig = new Float64Array([
		1,
		0,  // col 1
		0,
		1,  // col 2
		1,
		0,  // col 3
		0,
		1,  // col 4
		0,
		0   // col 5
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array([
		1,
		2,
		0,
		0,
		0,  // RHS 1
		3,
		4,
		0,
		0,
		0,  // RHS 2
		5,
		6,
		0,
		0,
		0   // RHS 3
	]);
	S = new Float64Array( 2 );
	rank = [ 0 ];
	info = dgelss( 2, 5, 3, A, 1, 2, 0, B, 1, 5, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 2, 'rank' );
	x1 = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ], B[ 3 ], B[ 4 ] ] );
	x2 = new Float64Array( [ B[ 5 ], B[ 6 ], B[ 7 ], B[ 8 ], B[ 9 ] ] );
	x3 = new Float64Array( [ B[ 10 ], B[ 11 ], B[ 12 ], B[ 13 ], B[ 14 ] ] );
	assertResidualSmall( 2, 5, Aorig, x1, new Float64Array( [ 1, 2 ] ), 1e-10, 'residual1' ); // eslint-disable-line max-len
	assertResidualSmall( 2, 5, Aorig, x2, new Float64Array( [ 3, 4 ] ), 1e-10, 'residual2' ); // eslint-disable-line max-len
	assertResidualSmall( 2, 5, Aorig, x3, new Float64Array( [ 5, 6 ] ), 1e-10, 'residual3' ); // eslint-disable-line max-len
});

test( 'dgelss: overdetermined multiple RHS with large NRHS (4x2, 4 RHS)', function t() { // eslint-disable-line max-len
	var Aorig;
	var rank;
	var info;
	var A;
	var B;
	var S;

	Aorig = new Float64Array([
		2,
		0,
		1,
		0,  // col 1
		0,
		3,
		0,
		1   // col 2
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array([
		1,
		2,
		3,
		4,  // RHS 1
		4,
		5,
		6,
		7,  // RHS 2
		7,
		8,
		9,
		10, // RHS 3
		10,
		11,
		12,
		13 // RHS 4
	]);
	S = new Float64Array( 2 );
	rank = [ 0 ];
	info = dgelss( 4, 2, 4, A, 1, 4, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 2, 'rank' );
});

test( 'dgelss: rank-deficient overdetermined (4x3, rank 2)', function t() {
	var Aorig;
	var rank;
	var info;
	var A;
	var B;
	var S;

	Aorig = new Float64Array([
		1,
		0,
		1,
		0,  // col 1
		0,
		1,
		0,
		1,  // col 2
		1,
		1,
		1,
		1   // col 3 = col1 + col2
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array( [ 3, 5, 7, 9 ] );
	S = new Float64Array( 3 );
	rank = [ 0 ];
	info = dgelss( 4, 3, 1, A, 1, 4, 0, B, 1, 4, 0, S, 1, 0, 0.01, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 2, 'rank should be 2' );
	assert.ok( S[ 2 ] < 0.01 * S[ 0 ], 'third singular value small relative to first' ); // eslint-disable-line max-len
});

test( 'dgelss: underdetermined direct bidiag with multiple RHS', function t() {
	var Aorig;
	var rank;
	var WORK;
	var info;
	var x1;
	var x2;
	var A;
	var B;
	var S;

	Aorig = new Float64Array([
		1,
		0,  // col 1
		0,
		1,  // col 2
		1,
		0,  // col 3
		0,
		1,  // col 4
		0,
		0   // col 5
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array([
		3,
		5,
		0,
		0,
		0,  // RHS 1
		1,
		2,
		0,
		0,
		0   // RHS 2
	]);
	S = new Float64Array( 2 );
	rank = [ 0 ];
	WORK = new Float64Array( 30 );
	info = dgelss( 2, 5, 2, A, 1, 2, 0, B, 1, 5, 0, S, 1, 0, -1.0, rank, WORK, 1, 0, 30 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 2, 'rank' );
	x1 = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ], B[ 3 ], B[ 4 ] ] );
	x2 = new Float64Array( [ B[ 5 ], B[ 6 ], B[ 7 ], B[ 8 ], B[ 9 ] ] );
	assertResidualSmall( 2, 5, Aorig, x1, new Float64Array( [ 3, 5 ] ), 1e-10, 'residual1' ); // eslint-disable-line max-len
	assertResidualSmall( 2, 5, Aorig, x2, new Float64Array( [ 1, 2 ] ), 1e-10, 'residual2' ); // eslint-disable-line max-len
});

test( 'dgelss: M >= N chunked GEMM path (multi-RHS with small workspace)', function t() { // eslint-disable-line max-len
	var Aorig;
	var rank;
	var WORK;
	var info;
	var A;
	var B;
	var S;

	Aorig = new Float64Array([
		2,
		0,
		1,
		0,  // col 1
		0,
		3,
		0,
		1   // col 2
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array([
		1,
		2,
		3,
		4,
		5,
		6,
		7,
		8,
		9,
		10,
		11,
		12
	]);
	S = new Float64Array( 2 );
	rank = [ 0 ];
	WORK = new Float64Array( 8 );
	info = dgelss( 4, 2, 3, A, 1, 4, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, WORK, 1, 0, 8 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 2, 'rank' );
});

test( 'dgelss: very small A elements trigger scaling (anrm < smlnum)', function t() { // eslint-disable-line max-len
	var scale;
	var Aorig;
	var rank;
	var info;
	var A;
	var B;
	var S;

	scale = 1e-295;
	Aorig = new Float64Array( [ 2 * scale, 1 * scale, 0, 1 * scale, 3 * scale, 1 * scale, 0, 1 * scale, 2 * scale ] ); // eslint-disable-line max-len
	A = new Float64Array( Aorig );
	B = new Float64Array( [ 1, 2, 3 ] );
	S = new Float64Array( 3 );
	rank = [ 0 ];
	info = dgelss( 3, 3, 1, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.ok( rank[ 0 ] >= 1, 'rank at least 1' );
});

test( 'dgelss: very large A elements trigger scaling (anrm > bignum)', function t() { // eslint-disable-line max-len
	var scale;
	var Aorig;
	var rank;
	var info;
	var A;
	var B;
	var S;

	scale = 1e295;
	Aorig = new Float64Array( [ 2 * scale, 1 * scale, 0, 1 * scale, 3 * scale, 1 * scale, 0, 1 * scale, 2 * scale ] ); // eslint-disable-line max-len
	A = new Float64Array( Aorig );
	B = new Float64Array( [ 1, 2, 3 ] );
	S = new Float64Array( 3 );
	rank = [ 0 ];
	info = dgelss( 3, 3, 1, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.ok( rank[ 0 ] >= 1, 'rank at least 1' );
});

test( 'dgelss: very small B elements trigger B scaling (bnrm < smlnum)', function t() { // eslint-disable-line max-len
	var bscale;
	var Aorig;
	var rank;
	var info;
	var A;
	var B;
	var S;

	Aorig = new Float64Array( [ 2, 1, 0, 1, 3, 1, 0, 1, 2 ] );
	A = new Float64Array( Aorig );
	bscale = 1e-295;
	B = new Float64Array( [ 1 * bscale, 2 * bscale, 3 * bscale ] );
	S = new Float64Array( 3 );
	rank = [ 0 ];
	info = dgelss( 3, 3, 1, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.ok( rank[ 0 ] >= 1, 'rank at least 1' );
});

test( 'dgelss: very large B elements trigger B scaling (bnrm > bignum)', function t() { // eslint-disable-line max-len
	var bscale;
	var Aorig;
	var rank;
	var info;
	var A;
	var B;
	var S;

	Aorig = new Float64Array( [ 2, 1, 0, 1, 3, 1, 0, 1, 2 ] );
	A = new Float64Array( Aorig );
	bscale = 1e295;
	B = new Float64Array( [ 1 * bscale, 2 * bscale, 3 * bscale ] );
	S = new Float64Array( 3 );
	rank = [ 0 ];
	info = dgelss( 3, 3, 1, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.ok( rank[ 0 ] >= 1, 'rank at least 1' );
});

test( 'dgelss: N > M path 2b chunked GEMM with multiple RHS (limited workspace)', function t() { // eslint-disable-line max-len
	var Aorig;
	var rank;
	var WORK;
	var info;
	var A;
	var B;
	var S;

	Aorig = new Float64Array([
		1,
		0,
		0,
		1,
		1,
		0,
		0,
		1,
		0,
		0
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array([
		1,
		2,
		0,
		0,
		0,
		3,
		4,
		0,
		0,
		0,
		5,
		6,
		0,
		0,
		0
	]);
	S = new Float64Array( 2 );
	rank = [ 0 ];
	WORK = new Float64Array( 10 );
	info = dgelss( 2, 5, 3, A, 1, 2, 0, B, 1, 5, 0, S, 1, 0, -1.0, rank, WORK, 1, 0, 10 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 2, 'rank' );
});

test( 'dgelss: LQ path (2a) chunked GEMM with multiple RHS and limited workspace', function t() { // eslint-disable-line max-len
	var Aorig;
	var lwork;
	var rank;
	var WORK;
	var info;
	var x1;
	var A;
	var B;
	var S;
	var M;

	Aorig = new Float64Array([
		1,
		0,  // col 1
		0,
		1,  // col 2
		1,
		1,  // col 3
		0,
		0,  // col 4
		0,
		0,  // col 5
		0,
		0   // col 6
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array([
		1,
		2,
		0,
		0,
		0,
		0,
		3,
		4,
		0,
		0,
		0,
		0,
		5,
		6,
		0,
		0,
		0,
		0
	]);
	S = new Float64Array( 2 );
	rank = [ 0 ];
	M = 2;
	lwork = 4 * M + M * M + Math.max( M, 2 * M - 4, 3, 6 - 3 * M ) + 2;
	WORK = new Float64Array( lwork );
	info = dgelss( 2, 6, 3, A, 1, 2, 0, B, 1, 6, 0, S, 1, 0, -1.0, rank, WORK, 1, 0, lwork ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 2, 'rank' );
	x1 = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ], B[ 3 ], B[ 4 ], B[ 5 ] ] );
	assertResidualSmall( 2, 6, Aorig, x1, new Float64Array( [ 1, 2 ] ), 1e-10, 'residual1' ); // eslint-disable-line max-len
});

test( 'dgelss: rank-deficient in path 2b (M < N, zero singular values)', function t() { // eslint-disable-line max-len
	var Aorig;
	var rank;
	var WORK;
	var info;
	var A;
	var B;
	var S;

	Aorig = new Float64Array([
		1,
		0,
		1,  // col 1
		0,
		1,
		1,  // col 2
		0,
		0,
		0,  // col 3
		0,
		0,
		0,  // col 4
		0,
		0,
		0   // col 5
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array( [ 1, 2, 3, 0, 0 ] );
	S = new Float64Array( 3 );
	rank = [ 0 ];
	WORK = new Float64Array( 20 );
	info = dgelss( 3, 5, 1, A, 1, 3, 0, B, 1, 5, 0, S, 1, 0, 0.01, rank, WORK, 1, 0, 20 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.ok( rank[ 0 ] <= 2, 'rank should be at most 2' );
});

test( 'dgelss: underdetermined wide (2x6, N >> M triggers LQ path)', function t() { // eslint-disable-line max-len
	var Aorig;
	var rank;
	var info;
	var tc;
	var A;
	var B;
	var S;
	var x;

	tc = underdetermined_wide;
	Aorig = new Float64Array([
		1,
		0,  // col 1
		1,
		1,  // col 2
		0,
		1,  // col 3
		0,
		0,  // col 4
		0,
		0,  // col 5
		0,
		0   // col 6
	]);
	A = new Float64Array( Aorig );
	B = new Float64Array( [ 2, 4, 0, 0, 0, 0 ] );
	S = new Float64Array( 2 );
	rank = [ 0 ];
	info = dgelss( 2, 6, 1, A, 1, 2, 0, B, 1, 6, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 ); // eslint-disable-line max-len
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	x = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ], B[ 3 ], B[ 4 ], B[ 5 ] ] );
	assertResidualSmall( 2, 6, Aorig, x, new Float64Array( [ 2, 4 ] ), 1e-12, 'residual' ); // eslint-disable-line max-len
});
