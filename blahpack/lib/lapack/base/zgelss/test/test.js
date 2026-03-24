'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgelss = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgelss.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'zgelss: main export is a function', function t() {
	assert.strictEqual( typeof zgelss, 'function' );
});

test( 'zgelss: overdetermined full rank (4x2), single RHS', function t() {
	var tc = findCase( 'overdetermined_full_rank' );
	// A = [1+i, 2; 3, 4-i; 5+2i, 6; 7, 8+i] (column-major)
	var A = new Complex128Array( [
		1, 1, 3, 0, 5, 2, 7, 0,
		2, 0, 4, -1, 6, 0, 8, 1
	] );
	// B must have LDB >= max(M,N) = 4
	var B = new Complex128Array( [
		1, 1, 2, 0, 3, -1, 4, 0
	] );
	var S = new Float64Array( 2 );
	var rank = [ 0 ];
	var info = zgelss( 4, 2, 1, A, 1, 4, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, tc.info );
	assert.strictEqual( rank[ 0 ], tc.rank );
	assertArrayClose( Array.from( S ), tc.s, 1e-10, 's' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ).slice( 0, 4 ), tc.x, 1e-10, 'x' );
});

test( 'zgelss: overdetermined rank-deficient (4x2)', function t() {
	var tc = findCase( 'overdetermined_rank_deficient' );
	// Rank-1 matrix: col2 = 2*col1
	var A = new Complex128Array( [
		1, 0, 2, 1, 3, 0, 4, -1,
		2, 0, 4, 2, 6, 0, 8, -2
	] );
	var B = new Complex128Array( [
		1, 0, 2, 1, 3, 0, 4, -1
	] );
	var S = new Float64Array( 2 );
	var rank = [ 0 ];
	var info = zgelss( 4, 2, 1, A, 1, 4, 0, B, 1, 4, 0, S, 1, 0, 0.01, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, tc.info );
	assert.strictEqual( rank[ 0 ], tc.rank );
	assertArrayClose( Array.from( S ), tc.s, 1e-10, 's' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ).slice( 0, 4 ), tc.x, 1e-10, 'x' );
});

test( 'zgelss: underdetermined (2x4), single RHS', function t() {
	var tc = findCase( 'underdetermined' );
	// A = [1 0 0 0; 0 1+i 0 0]
	var A = new Complex128Array( [
		1, 0, 0, 0,
		0, 0, 1, 1,
		0, 0, 0, 0,
		0, 0, 0, 0
	] );
	// B must be max(M,N)=4 by NRHS=1, LDB=4
	var B = new Complex128Array( 4 );
	var Bv = reinterpret( B, 0 );
	Bv[ 0 ] = 1.0; Bv[ 1 ] = 1.0; Bv[ 2 ] = 2.0; Bv[ 3 ] = 0.0;
	var S = new Float64Array( 2 );
	var rank = [ 0 ];
	var info = zgelss( 2, 4, 1, A, 1, 2, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, tc.info );
	assert.strictEqual( rank[ 0 ], tc.rank );
	assertArrayClose( Array.from( S ), tc.s, 1e-10, 's' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ).slice( 0, 8 ), tc.x, 1e-10, 'x' );
});

test( 'zgelss: square 3x3, single RHS', function t() {
	var tc = findCase( 'square_3x3' );
	// Hermitian pd matrix
	var A = new Complex128Array( [
		4, 0, 1, 1, 0, 0,
		1, -1, 5, 0, 2, 1,
		0, 0, 2, -1, 6, 0
	] );
	var B = new Complex128Array( [
		1, 1, 2, 0, 3, -1
	] );
	var S = new Float64Array( 3 );
	var rank = [ 0 ];
	var info = zgelss( 3, 3, 1, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, tc.info );
	assert.strictEqual( rank[ 0 ], tc.rank );
	assertArrayClose( Array.from( S ), tc.s, 1e-10, 's' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ).slice( 0, 6 ), tc.x, 1e-10, 'x' );
});

test( 'zgelss: multiple RHS (3x3, 2 RHS)', function t() {
	var tc = findCase( 'multiple_rhs' );
	var A = new Complex128Array( [
		4, 0, 1, 0, 0, 0,
		1, 0, 5, 0, 2, 0,
		0, 0, 2, 0, 6, 0
	] );
	// B: 3-by-2 column-major
	var B = new Complex128Array( [
		1, 1, 2, 0, 3, 0,
		2, 0, 3, -1, 4, 1
	] );
	var S = new Float64Array( 3 );
	var rank = [ 0 ];
	var info = zgelss( 3, 3, 2, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, tc.info );
	assert.strictEqual( rank[ 0 ], tc.rank );
	assertArrayClose( Array.from( S ), tc.s, 1e-10, 's' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ).slice( 0, 12 ), tc.x, 1e-10, 'x' );
});

test( 'zgelss: M=0 edge case', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 3 );
	var S = new Float64Array( 1 );
	var rank = [ 0 ];
	var info = zgelss( 0, 3, 1, A, 1, 1, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, tc.info );
	assert.strictEqual( rank[ 0 ], tc.rank );
});

test( 'zgelss: N=0 edge case', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 3 );
	var B = new Complex128Array( 3 );
	var S = new Float64Array( 1 );
	var rank = [ 0 ];
	var info = zgelss( 3, 0, 1, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, tc.info );
	assert.strictEqual( rank[ 0 ], tc.rank );
});

test( 'zgelss: overdetermined tall 6x2 (QR path)', function t() {
	var tc = findCase( 'overdetermined_tall' );
	// M=6, N=2: M/N = 3 > 1.6, triggers QR precondition path
	var A = new Complex128Array( [
		1, 0, 0, 1, 1, 1, 2, 0, 1, -1, 0, 0,
		0, 0, 1, 0, 1, -1, 1, 1, 2, 0, 0, 0
	] );
	var B = new Complex128Array( [
		1, 0, 1, 1, 2, 0, 3, -1, 3, 0, 0, 0
	] );
	var S = new Float64Array( 2 );
	var rank = [ 0 ];
	var info = zgelss( 6, 2, 1, A, 1, 6, 0, B, 1, 6, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, tc.info );
	assert.strictEqual( rank[ 0 ], tc.rank );
	assertArrayClose( Array.from( S ), tc.s, 1e-10, 's' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ).slice( 0, 4 ), tc.x, 1e-10, 'x' );
});

test( 'zgelss: all-zero matrix', function t() {
	// Zero A should return rank=0, zero solution, zero singular values
	var A = new Complex128Array( 4 ); // 2x2 zero
	var B = new Complex128Array( [ 1, 0, 2, 0 ] );
	var S = new Float64Array( 2 );
	var rank = [ 0 ];
	var info = zgelss( 2, 2, 1, A, 1, 2, 0, B, 1, 2, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rank[ 0 ], 0 );
	assert.strictEqual( S[ 0 ], 0.0 );
	assert.strictEqual( S[ 1 ], 0.0 );
	// B should be zeroed out
	var Bv = reinterpret( B, 0 );
	assert.strictEqual( Bv[ 0 ], 0.0 );
	assert.strictEqual( Bv[ 1 ], 0.0 );
	assert.strictEqual( Bv[ 2 ], 0.0 );
	assert.strictEqual( Bv[ 3 ], 0.0 );
});

test( 'zgelss: underdetermined N>M path 2b (small workspace, direct bidiag)', function t() {
	// Use M=3, N=4 so N > M but not by enough for LQ path (N < mnthr=round(1.6*3)=5)
	// This takes Path 2b (direct bidiagonal reduction)
	var A = new Complex128Array( [
		2, 0, 1, 1, 0, 0,
		1, -1, 3, 0, 1, 0,
		0, 0, 1, 1, 4, 0,
		0, 0, 0, 0, 1, -1
	] );
	var B = new Complex128Array( 4 );
	var Bv = reinterpret( B, 0 );
	Bv[ 0 ] = 1.0; Bv[ 1 ] = 0.0;
	Bv[ 2 ] = 2.0; Bv[ 3 ] = 1.0;
	Bv[ 4 ] = 3.0; Bv[ 5 ] = -1.0;
	var S = new Float64Array( 3 );
	var rank = [ 0 ];
	var info = zgelss( 3, 4, 1, A, 1, 3, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( rank[ 0 ] > 0, 'rank should be positive' );
	// Verify solution is sensible: S should be decreasing and positive
	assert.ok( S[ 0 ] >= S[ 1 ], 'singular values should be decreasing' );
	assert.ok( S[ 1 ] >= S[ 2 ], 'singular values should be decreasing' );
});

test( 'zgelss: overdetermined nrhs>1 chunk path (M >= N)', function t() {
	// 3x3, 2 RHS with explicitly small workspace to force chunk path
	var A = new Complex128Array( [
		4, 0, 1, 0, 0, 0,
		1, 0, 5, 0, 2, 0,
		0, 0, 2, 0, 6, 0
	] );
	var B = new Complex128Array( [
		1, 0, 2, 0, 3, 0,
		4, 0, 5, 0, 6, 0
	] );
	var S = new Float64Array( 3 );
	var rank = [ 0 ];
	// Use small workspace to force the nrhs>1 chunk path
	var WORK = new Complex128Array( 10 );
	var RWORK = new Float64Array( 20 );
	var info = zgelss( 3, 3, 2, A, 1, 3, 0, B, 1, 3, 0, S, 1, 0, -1.0, rank, WORK, 1, 0, 10, RWORK, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rank[ 0 ], 3 );
});

test( 'zgelss: underdetermined path 2b with nrhs>1', function t() {
	// N=4, M=3, nrhs=2 — hits path 2b nrhs>1
	var A = new Complex128Array( [
		2, 0, 1, 0, 0, 0,
		1, 0, 3, 0, 1, 0,
		0, 0, 1, 0, 4, 0,
		0, 0, 0, 0, 1, 0
	] );
	var B = new Complex128Array( [
		1, 0, 2, 0, 3, 0, 0, 0,
		4, 0, 5, 0, 6, 0, 0, 0
	] );
	var S = new Float64Array( 3 );
	var rank = [ 0 ];
	var info = zgelss( 3, 4, 2, A, 1, 3, 0, B, 1, 4, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( rank[ 0 ] > 0 );
});

test( 'zgelss: path 2a nrhs>1 (LQ path, multiple RHS)', function t() {
	// M=2, N=6, nrhs=2 — hits LQ path with multiple RHS
	var A = new Complex128Array( [
		1, 0, 0, 0,
		0, 0, 1, 0,
		1, 0, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 0,
		0, 0, 0, 0
	] );
	var B = new Complex128Array( [
		1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		3, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0
	] );
	var S = new Float64Array( 2 );
	var rank = [ 0 ];
	var info = zgelss( 2, 6, 2, A, 1, 2, 0, B, 1, 6, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.ok( rank[ 0 ] > 0 );
});

test( 'zgelss: tiny A norm (scale up path)', function t() {
	// Matrix with very small entries to trigger the smlnum scaling path
	var scale = 1e-300;
	var A = new Complex128Array( [
		4 * scale, 0, 1 * scale, 0,
		1 * scale, 0, 3 * scale, 0
	] );
	var B = new Complex128Array( [
		1 * scale, 0, 2 * scale, 0
	] );
	var S = new Float64Array( 2 );
	var rank = [ 0 ];
	var info = zgelss( 2, 2, 1, A, 1, 2, 0, B, 1, 2, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rank[ 0 ], 2 );
});

test( 'zgelss: large A norm (scale down path)', function t() {
	// Matrix with very large entries to trigger the bignum scaling path
	var scale = 1e295;
	var A = new Complex128Array( [
		4 * scale, 0, 1 * scale, 0,
		1 * scale, 0, 3 * scale, 0
	] );
	var B = new Complex128Array( [
		1 * scale, 0, 2 * scale, 0
	] );
	var S = new Float64Array( 2 );
	var rank = [ 0 ];
	var info = zgelss( 2, 2, 1, A, 1, 2, 0, B, 1, 2, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rank[ 0 ], 2 );
});

test( 'zgelss: underdetermined wide 2x6 (LQ path)', function t() {
	var tc = findCase( 'underdetermined_wide' );
	// M=2, N=6
	var A = new Complex128Array( [
		1, 0, 0, 0,
		0, 0, 1, -1,
		1, 1, 0, 0,
		0, 0, 1, 0,
		0, 0, 0, 0,
		0, 0, 0, 0
	] );
	// B must have LDB=6 (max(M,N))
	var B = new Complex128Array( 6 );
	var Bv = reinterpret( B, 0 );
	Bv[ 0 ] = 2.0; Bv[ 1 ] = 1.0; Bv[ 2 ] = 4.0; Bv[ 3 ] = -1.0;
	var S = new Float64Array( 2 );
	var rank = [ 0 ];
	var info = zgelss( 2, 6, 1, A, 1, 2, 0, B, 1, 6, 0, S, 1, 0, -1.0, rank, null, 1, 0, 0, null, 1, 0 );
	assert.strictEqual( info, tc.info );
	assert.strictEqual( rank[ 0 ], tc.rank );
	assertArrayClose( Array.from( S ), tc.s, 1e-10, 's' );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ).slice( 0, 12 ), tc.x, 1e-10, 'x' );
});
