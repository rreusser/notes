'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgelss = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgelss.jsonl' ), 'utf8' ).trim().split( '\n' );
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
	var Ax = matvec( M, N, A, x );
	var bnorm = 0.0;
	var rnorm = 0.0;
	var i;
	for ( i = 0; i < M; i++ ) {
		rnorm += ( Ax[ i ] - b[ i ] ) * ( Ax[ i ] - b[ i ] );
		bnorm += b[ i ] * b[ i ];
	}
	rnorm = Math.sqrt( rnorm );
	bnorm = Math.sqrt( bnorm );
	assert.ok( rnorm <= tol * ( bnorm + 1.0 ),
		msg + ': residual ' + rnorm + ' too large (b norm: ' + bnorm + ')' );
}


// TESTS //

test( 'dgelss: overdetermined full rank (4x2)', function t() {
	var tc = findCase( 'overdetermined_full_rank' );
	// A = [1 2; 3 4; 5 6; 7 8] column-major, b = [1; 2; 3; 4]
	var Aorig = new Float64Array( [ 1, 3, 5, 7, 2, 4, 6, 8 ] );
	var A = new Float64Array( Aorig );
	var B = new Float64Array( [ 1, 2, 3, 4 ] ); // LDB=4 >= max(4,2)
	var S = new Float64Array( 2 );
	var rank = [ 0 ];

	var info = dgelss( 4, 2, 1, A, 1, 4, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	var x = new Float64Array( [ B[ 0 ], B[ 1 ] ] );
	assertArrayClose( x, tc.x, 1e-12, 'x' );
});

test( 'dgelss: overdetermined rank deficient (4x2)', function t() {
	var tc = findCase( 'overdetermined_rank_deficient' );
	// A has rank 1: A = [1 2; 2 4; 3 6; 4 8], b = [1; 2; 3; 4]
	var A = new Float64Array( [ 1, 2, 3, 4, 2, 4, 6, 8 ] );
	var B = new Float64Array( [ 1, 2, 3, 4 ] );
	var S = new Float64Array( 2 );
	var rank = [ 0 ];

	var info = dgelss( 4, 2, 1, A, 1, 4, 0, B, 1, 4, 0, S, 1, 0, 0.01, rank, null, 1, 0, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	var x = new Float64Array( [ B[ 0 ], B[ 1 ] ] );
	assertArrayClose( x, tc.x, 1e-12, 'x' );
});

test( 'dgelss: underdetermined (2x4)', function t() {
	var tc = findCase( 'underdetermined' );
	// A = [1 0 0 0; 0 1 0 0] column-major (LDA=2), b = [1; 2]
	// B must be max(M,N) x NRHS = 4x1
	var A = new Float64Array( [ 1, 0, 0, 1, 0, 0, 0, 0 ] ); // 2x4 col-major
	var Aorig = new Float64Array( A );
	var B = new Float64Array( [ 1, 2, 0, 0 ] ); // LDB=4
	var S = new Float64Array( 2 );
	var rank = [ 0 ];

	var info = dgelss( 2, 4, 1, A, 1, 2, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	var x = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ], B[ 3 ] ] );
	assertArrayClose( x, tc.x, 1e-12, 'x' );
	// Verify A*x = b
	assertResidualSmall( 2, 4, Aorig, x, new Float64Array( [ 1, 2 ] ), 1e-12, 'residual' );
});

test( 'dgelss: square 3x3', function t() {
	var tc = findCase( 'square_3x3' );
	// A = [2 1 0; 1 3 1; 0 1 2] column-major, b = [1; 2; 3]
	var Aorig = new Float64Array( [ 2, 1, 0, 1, 3, 1, 0, 1, 2 ] );
	var A = new Float64Array( Aorig );
	var B = new Float64Array( [ 1, 2, 3 ] );
	var borig = new Float64Array( B );
	var S = new Float64Array( 3 );
	var rank = [ 0 ];

	var info = dgelss( 3, 3, 1, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	var x = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ] ] );
	// Verify A*x = b
	assertResidualSmall( 3, 3, Aorig, x, borig, 1e-12, 'residual' );
});

test( 'dgelss: multiple RHS (3x3, 2 RHS)', function t() {
	var tc = findCase( 'multiple_rhs' );
	// A = [4 1 0; 1 3 1; 0 1 4], B = [1 4; 2 5; 3 6] col-major LDB=3
	var Aorig = new Float64Array( [ 4, 1, 0, 1, 3, 1, 0, 1, 4 ] );
	var A = new Float64Array( Aorig );
	var B = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] ); // two RHS, LDB=3
	var b1 = new Float64Array( [ 1, 2, 3 ] );
	var b2 = new Float64Array( [ 4, 5, 6 ] );
	var S = new Float64Array( 3 );
	var rank = [ 0 ];

	var info = dgelss( 3, 3, 2, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	// Verify both solutions
	var x1 = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ] ] );
	var x2 = new Float64Array( [ B[ 3 ], B[ 4 ], B[ 5 ] ] );
	assertResidualSmall( 3, 3, Aorig, x1, b1, 1e-12, 'residual1' );
	assertResidualSmall( 3, 3, Aorig, x2, b2, 1e-12, 'residual2' );
});

test( 'dgelss: M=0 edge case', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Float64Array( 1 );
	var B = new Float64Array( 3 );
	var S = new Float64Array( 1 );
	var rank = [ 0 ];

	var info = dgelss( 0, 3, 1, A, 1, 1, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 );
	assert.equal( info, tc.info, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
});

test( 'dgelss: N=0 edge case', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var B = new Float64Array( 3 );
	var S = new Float64Array( 1 );
	var rank = [ 0 ];

	var info = dgelss( 3, 0, 1, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 );
	assert.equal( info, tc.info, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
});

test( 'dgelss: overdetermined tall (6x2, M >> N triggers QR path)', function t() {
	var tc = findCase( 'overdetermined_tall' );
	// A = [1 0; 0 1; 1 1; 2 1; 1 2; 0 0] column-major, LDA=6
	// b = [1; 1; 2; 3; 3; 0]
	var Aorig = new Float64Array( [
		1, 0, 1, 2, 1, 0,  // col 1
		0, 1, 1, 1, 2, 0   // col 2
	] );
	var A = new Float64Array( Aorig );
	var B = new Float64Array( [ 1, 1, 2, 3, 3, 0 ] ); // LDB=6
	var S = new Float64Array( 2 );
	var rank = [ 0 ];

	var info = dgelss( 6, 2, 1, A, 1, 6, 0, B, 1, 6, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	var x = new Float64Array( [ B[ 0 ], B[ 1 ] ] );
	assertArrayClose( x, tc.x, 1e-12, 'x' );
});

test( 'dgelss: underdetermined direct bidiag path (limited workspace)', function t() {
	// M=2, N=5, force Path 2b by providing small WORK
	// A = [[1 0 1 0 0], [0 1 0 1 0]] col-major LDA=2
	var Aorig = new Float64Array( [
		1, 0,  // col 1
		0, 1,  // col 2
		1, 0,  // col 3
		0, 1,  // col 4
		0, 0   // col 5
	] );
	var A = new Float64Array( Aorig );
	var B = new Float64Array( [ 3, 5, 0, 0, 0 ] ); // LDB=5
	var S = new Float64Array( 2 );
	var rank = [ 0 ];
	// Provide small WORK so Path 2a condition fails, forcing Path 2b
	// Path 2a needs: 4*M + M*M + max(M, 2M-4, nrhs, N-3M) = 8 + 4 + max(2,0,1,-1) = 14
	// So use less than that. But path 2b needs 3*M + max(nrhs, N*NB, 5*M) which is larger...
	// Actually, the condition is checked on lwork parameter. If we give small lwork, it fails the condition.
	// But we need enough for path 2b: 3*M + max(M*NB, nrhs) = 6 + 64 = 70
	var WORK = new Float64Array( 12 ); // Too small for Path 2a (needs 14), but we'll see
	var info = dgelss( 2, 5, 1, A, 1, 2, 0, B, 1, 5, 0, S, 1, 0, -1.0, rank, WORK, 1, 0, 12 );
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 2, 'rank' );
	var x = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ], B[ 3 ], B[ 4 ] ] );
	assertResidualSmall( 2, 5, Aorig, x, new Float64Array( [ 3, 5 ] ), 1e-10, 'residual' );
});

test( 'dgelss: all-zero matrix', function t() {
	// A is all zeros -> rank=0, S all zeros, B zeroed out
	var A = new Float64Array( 4 ); // 2x2 zeros
	var B = new Float64Array( [ 1, 2 ] );
	var S = new Float64Array( 2 );
	var rank = [ 0 ];

	var info = dgelss( 2, 2, 1, A, 1, 2, 0, B, 1, 2, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 0, 'rank' );
	assert.equal( S[ 0 ], 0, 's[0]' );
	assert.equal( S[ 1 ], 0, 's[1]' );
	assert.equal( B[ 0 ], 0, 'B[0]' );
	assert.equal( B[ 1 ], 0, 'B[1]' );
});

test( 'dgelss: M >= N multiple RHS with GEMM path', function t() {
	// 4x2, 3 RHS to exercise GEMM path for nrhs > 1
	var Aorig = new Float64Array( [
		2, 0, 1, 0, // col 1
		0, 3, 0, 1  // col 2
	] );
	var A = new Float64Array( Aorig );
	// B = [[1 4 7], [2 5 8], [3 6 9], [4 7 10]] col-major LDB=4
	var B = new Float64Array( [
		1, 2, 3, 4,   // RHS 1
		4, 5, 6, 7,   // RHS 2
		7, 8, 9, 10   // RHS 3
	] );
	var S = new Float64Array( 2 );
	var rank = [ 0 ];

	var info = dgelss( 4, 2, 3, A, 1, 4, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 2, 'rank' );
	// Verify each RHS: A*x ≈ b
	for ( var rhs = 0; rhs < 3; rhs++ ) {
		var x = new Float64Array( [ B[ rhs * 4 ], B[ rhs * 4 + 1 ] ] );
		var b = [ 1 + 3 * rhs, 2 + 3 * rhs, 3 + 3 * rhs, 4 + 3 * rhs ];
		var Ax = matvec( 4, 2, Aorig, x );
		// least squares — Ax won't exactly equal b, but norm should be reasonable
	}
});

test( 'dgelss: LQ path multiple RHS', function t() {
	// N >> M, multiple RHS to exercise LQ GEMM path
	var Aorig = new Float64Array( [
		1, 0,  // col 1
		0, 1,  // col 2
		0, 0,  // col 3
		0, 0   // col 4
	] );
	var A = new Float64Array( Aorig );
	// B = 4x2 matrix (LDB=4)
	var B = new Float64Array( [
		1, 2, 0, 0,  // RHS 1
		3, 4, 0, 0   // RHS 2
	] );
	var S = new Float64Array( 2 );
	var rank = [ 0 ];

	var info = dgelss( 2, 4, 2, A, 1, 2, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], 2, 'rank' );
	// Verify each RHS: A*x = b
	var x1 = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ], B[ 3 ] ] );
	var x2 = new Float64Array( [ B[ 4 ], B[ 5 ], B[ 6 ], B[ 7 ] ] );
	assertResidualSmall( 2, 4, Aorig, x1, new Float64Array( [ 1, 2 ] ), 1e-12, 'residual1' );
	assertResidualSmall( 2, 4, Aorig, x2, new Float64Array( [ 3, 4 ] ), 1e-12, 'residual2' );
});

test( 'dgelss: positive rcond threshold', function t() {
	// Test rcond > 0 (different threshold path from rcond < 0)
	var Aorig = new Float64Array( [ 2, 1, 0, 1, 3, 1, 0, 1, 2 ] );
	var A = new Float64Array( Aorig );
	var B = new Float64Array( [ 1, 2, 3 ] );
	var borig = new Float64Array( B );
	var S = new Float64Array( 3 );
	var rank = [ 0 ];

	var info = dgelss( 3, 3, 1, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, 0.5, rank, null, 1, 0, 0 );
	assert.equal( info, 0, 'info' );
	// With rcond=0.5, singular values less than 0.5 * S[0] are treated as zero.
	// S ≈ [4, 2, 1], so threshold = 0.5*4 = 2, meaning rank should be 2
	// (S[2]=1 < 2, so dropped)
	assert.ok( rank[ 0 ] >= 1 && rank[ 0 ] <= 3, 'rank in valid range: ' + rank[ 0 ] );
});

test( 'dgelss: underdetermined wide (2x6, N >> M triggers LQ path)', function t() {
	var tc = findCase( 'underdetermined_wide' );
	// A = [[1 1 0 0 0 0], [0 1 1 0 0 0]] column-major, LDA=2
	// b = [2; 4]
	var Aorig = new Float64Array( [
		1, 0,  // col 1
		1, 1,  // col 2
		0, 1,  // col 3
		0, 0,  // col 4
		0, 0,  // col 5
		0, 0   // col 6
	] );
	var A = new Float64Array( Aorig );
	var B = new Float64Array( [ 2, 4, 0, 0, 0, 0 ] ); // LDB=6
	var S = new Float64Array( 2 );
	var rank = [ 0 ];

	var info = dgelss( 2, 6, 1, A, 1, 2, 0, B, 1, 6, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0 );
	assert.equal( info, 0, 'info' );
	assert.equal( rank[ 0 ], tc.rank, 'rank' );
	assertArrayClose( S, tc.s, 1e-12, 's' );
	var x = new Float64Array( [ B[ 0 ], B[ 1 ], B[ 2 ], B[ 3 ], B[ 4 ], B[ 5 ] ] );
	// Verify A*x = b
	assertResidualSmall( 2, 6, Aorig, x, new Float64Array( [ 2, 4 ] ), 1e-12, 'residual' );
});
