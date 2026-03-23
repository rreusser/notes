'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dgbtrf = require( '../../dgbtrf/lib/base.js' );
var dgbtrs = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgbtrs.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Multiply banded matrix A (in original form) times vector x and check A*x = b.
* A is specified in its original dense band storage before factorization.
*/
function bandMatVec( N, kl, ku, ABrows, x ) {
	// ABrows is a 2D array [LDAB][N] stored column-major as flat array with LDAB rows
	var LDAB = 2 * kl + ku + 1;
	var kv = ku + kl;
	var result = new Float64Array( N );
	var i;
	var j;
	var bandRow;

	for ( j = 0; j < N; j++ ) {
		for ( i = Math.max( 0, j - ku ); i < Math.min( N, j + kl + 1 ); i++ ) {
			// A(i,j) is stored at AB(kv + i - j, j) (0-based)
			bandRow = kv + i - j;
			result[ i ] += ABrows[ bandRow + j * LDAB ] * x[ j ];
		}
	}
	return result;
}


// TESTS //

test( 'dgbtrs: N=0 quick return', function t() {
	var AB = new Float64Array( 16 );
	var IPIV = new Int32Array( 4 );
	var B = new Float64Array( 4 );
	var info = dgbtrs( 'no-transpose', 0, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );
	assert.equal( info, 0 );
});

test( 'dgbtrs: NRHS=0 quick return', function t() {
	var AB = new Float64Array( 16 );
	var IPIV = new Int32Array( 4 );
	var B = new Float64Array( 4 );
	var info = dgbtrs( 'no-transpose', 4, 1, 1, 0, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );
	assert.equal( info, 0 );
});

test( 'dgbtrs: tridiag_4x4_1rhs (verify A*x = b)', function t() {
	var tc = findCase( 'tridiag_4x4_1rhs' );
	// Original A: tridiag [4 -1 0 0; -1 4 -1 0; 0 -1 4 -1; 0 0 -1 4]
	// KL=1, KU=1, KV=2, LDAB=4
	var AB_orig = new Float64Array( [
		0.0, 0.0, 4.0, -1.0,
		0.0, -1.0, 4.0, -1.0,
		0.0, -1.0, 4.0, -1.0,
		0.0, -1.0, 4.0, 0.0
	] );
	var AB = new Float64Array( AB_orig );
	var b = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var IPIV = new Int32Array( 4 );
	dgbtrf( 4, 4, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );

	// B is stored as a column vector (N x NRHS with LDB=N)
	var B = new Float64Array( b );
	var info = dgbtrs( 'no-transpose', 4, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );
	assert.equal( info, 0 );
	// Check fixture
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
	// Verify A*x = b
	var Ax = bandMatVec( 4, 1, 1, AB_orig, B );
	assertArrayClose( Array.from( Ax ), Array.from( b ), 1e-12, 'A*x=b' );
});

test( 'dgbtrs: tridiag_4x4_2rhs', function t() {
	var tc = findCase( 'tridiag_4x4_2rhs' );
	var AB_orig = new Float64Array( [
		0.0, 0.0, 4.0, -1.0,
		0.0, -1.0, 4.0, -1.0,
		0.0, -1.0, 4.0, -1.0,
		0.0, -1.0, 4.0, 0.0
	] );
	var AB = new Float64Array( AB_orig );
	var IPIV = new Int32Array( 4 );
	dgbtrf( 4, 4, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );

	// B is 4x2 column-major with LDB=4
	var B = new Float64Array( [
		1.0, 2.0, 3.0, 4.0,  // column 0
		4.0, 3.0, 2.0, 1.0   // column 1
	] );
	var b0 = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var b1 = new Float64Array( [ 4.0, 3.0, 2.0, 1.0 ] );
	var info = dgbtrs( 'no-transpose', 4, 1, 1, 2, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );
	assert.equal( info, 0 );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
	// Verify A*x = b for both columns
	var x0 = B.slice( 0, 4 );
	var x1 = B.slice( 4, 8 );
	assertArrayClose( Array.from( bandMatVec( 4, 1, 1, AB_orig, x0 ) ), Array.from( b0 ), 1e-12, 'A*x0=b0' );
	assertArrayClose( Array.from( bandMatVec( 4, 1, 1, AB_orig, x1 ) ), Array.from( b1 ), 1e-12, 'A*x1=b1' );
});

test( 'dgbtrs: tridiag_4x4_trans (verify A^T*x = b)', function t() {
	var tc = findCase( 'tridiag_4x4_trans' );
	var AB_orig = new Float64Array( [
		0.0, 0.0, 4.0, -1.0,
		0.0, -1.0, 4.0, -1.0,
		0.0, -1.0, 4.0, -1.0,
		0.0, -1.0, 4.0, 0.0
	] );
	var AB = new Float64Array( AB_orig );
	var IPIV = new Int32Array( 4 );
	dgbtrf( 4, 4, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );

	var B = new Float64Array( [ 1.0, 2.0, 3.0, 4.0 ] );
	var b = new Float64Array( B );
	var info = dgbtrs( 'transpose', 4, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 4, 0 );
	assert.equal( info, 0 );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
	// For symmetric A, A^T = A, so A*x should also = b
	var Ax = bandMatVec( 4, 1, 1, AB_orig, B );
	assertArrayClose( Array.from( Ax ), Array.from( b ), 1e-12, 'A^T*x=b' );
});

test( 'dgbtrs: pentadiag_5x5_1rhs (verify A*x = b)', function t() {
	var tc = findCase( 'pentadiag_5x5_1rhs' );
	// KL=2, KU=2, KV=4, LDAB=7
	var AB_orig = new Float64Array( 7 * 5 );
	AB_orig[ 4 ] = 6.0; AB_orig[ 5 ] = -2.0; AB_orig[ 6 ] = 1.0;
	AB_orig[ 7 + 3 ] = -2.0; AB_orig[ 7 + 4 ] = 6.0; AB_orig[ 7 + 5 ] = -2.0; AB_orig[ 7 + 6 ] = 1.0;
	AB_orig[ 14 + 2 ] = 1.0; AB_orig[ 14 + 3 ] = -2.0; AB_orig[ 14 + 4 ] = 6.0; AB_orig[ 14 + 5 ] = -2.0; AB_orig[ 14 + 6 ] = 1.0;
	AB_orig[ 21 + 2 ] = 1.0; AB_orig[ 21 + 3 ] = -2.0; AB_orig[ 21 + 4 ] = 6.0; AB_orig[ 21 + 5 ] = -2.0;
	AB_orig[ 28 + 2 ] = 1.0; AB_orig[ 28 + 3 ] = -2.0; AB_orig[ 28 + 4 ] = 6.0;
	var AB = new Float64Array( AB_orig );
	var IPIV = new Int32Array( 5 );
	dgbtrf( 5, 5, 2, 2, AB, 1, 7, 0, IPIV, 1, 0 );

	var b = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var B = new Float64Array( b );
	var info = dgbtrs( 'no-transpose', 5, 2, 2, 1, AB, 1, 7, 0, IPIV, 1, 0, B, 1, 5, 0 );
	assert.equal( info, 0 );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
	var Ax = bandMatVec( 5, 2, 2, AB_orig, B );
	assertArrayClose( Array.from( Ax ), Array.from( b ), 1e-12, 'A*x=b' );
});

test( 'dgbtrs: pentadiag_5x5_trans', function t() {
	var tc = findCase( 'pentadiag_5x5_trans' );
	var AB_orig = new Float64Array( 7 * 5 );
	AB_orig[ 4 ] = 6.0; AB_orig[ 5 ] = -2.0; AB_orig[ 6 ] = 1.0;
	AB_orig[ 7 + 3 ] = -2.0; AB_orig[ 7 + 4 ] = 6.0; AB_orig[ 7 + 5 ] = -2.0; AB_orig[ 7 + 6 ] = 1.0;
	AB_orig[ 14 + 2 ] = 1.0; AB_orig[ 14 + 3 ] = -2.0; AB_orig[ 14 + 4 ] = 6.0; AB_orig[ 14 + 5 ] = -2.0; AB_orig[ 14 + 6 ] = 1.0;
	AB_orig[ 21 + 2 ] = 1.0; AB_orig[ 21 + 3 ] = -2.0; AB_orig[ 21 + 4 ] = 6.0; AB_orig[ 21 + 5 ] = -2.0;
	AB_orig[ 28 + 2 ] = 1.0; AB_orig[ 28 + 3 ] = -2.0; AB_orig[ 28 + 4 ] = 6.0;
	var AB = new Float64Array( AB_orig );
	var IPIV = new Int32Array( 5 );
	dgbtrf( 5, 5, 2, 2, AB, 1, 7, 0, IPIV, 1, 0 );

	var b = new Float64Array( [ 1.0, 2.0, 3.0, 4.0, 5.0 ] );
	var B = new Float64Array( b );
	var info = dgbtrs( 'transpose', 5, 2, 2, 1, AB, 1, 7, 0, IPIV, 1, 0, B, 1, 5, 0 );
	assert.equal( info, 0 );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
});

test( 'dgbtrs: pivot_2x2 (verify A*x = b)', function t() {
	var tc = findCase( 'pivot_2x2' );
	// A = [1 2; 3 4], KL=1, KU=1, LDAB=4
	var AB_orig = new Float64Array( 4 * 2 );
	AB_orig[ 2 ] = 1.0; AB_orig[ 3 ] = 3.0;
	AB_orig[ 4 + 1 ] = 2.0; AB_orig[ 4 + 2 ] = 4.0;
	var AB = new Float64Array( AB_orig );
	var IPIV = new Int32Array( 2 );
	dgbtrf( 2, 2, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );

	var b = new Float64Array( [ 5.0, 11.0 ] );
	var B = new Float64Array( b );
	var info = dgbtrs( 'no-transpose', 2, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 2, 0 );
	assert.equal( info, 0 );
	assertArrayClose( Array.from( B ), tc.x, 1e-14, 'x' );
	// Verify: A*[1,2] = [1*1+2*2, 3*1+4*2] = [5, 11]
	assert.equal( B[ 0 ], 1.0 );
	assert.equal( B[ 1 ], 2.0 );
});

test( 'dgbtrs: pivot_2x2 transpose (verify A^T*x = b)', function t() {
	// A = [1 2; 3 4], KL=1, KU=1, LDAB=4
	// A^T = [1 3; 2 4]
	// b = [7, 10], x should be [1, 2] since A^T*[1,2] = [1+6, 2+8] = [7, 10]
	var AB = new Float64Array( 4 * 2 );
	AB[ 2 ] = 1.0; AB[ 3 ] = 3.0;
	AB[ 4 + 1 ] = 2.0; AB[ 4 + 2 ] = 4.0;
	var IPIV = new Int32Array( 2 );
	dgbtrf( 2, 2, 1, 1, AB, 1, 4, 0, IPIV, 1, 0 );

	var B = new Float64Array( [ 7.0, 10.0 ] );
	var info = dgbtrs( 'transpose', 2, 1, 1, 1, AB, 1, 4, 0, IPIV, 1, 0, B, 1, 2, 0 );
	assert.equal( info, 0 );
	assertClose( B[ 0 ], 1.0, 1e-14, 'x[0]' );
	assertClose( B[ 1 ], 2.0, 1e-14, 'x[1]' );
});

test( 'dgbtrs: KL=0 no-L path transpose', function t() {
	// A = [2 1; 0 3], KL=0, KU=1, LDAB=2
	// A^T = [2 0; 1 3]
	// b = [4, 7], x should be [2, 5/3] since A^T*x = [2*2, 1*2+3*5/3] = [4, 7]
	var AB = new Float64Array( [
		0.0, 2.0,
		1.0, 3.0
	] );
	var IPIV = new Int32Array( 2 );
	dgbtrf( 2, 2, 0, 1, AB, 1, 2, 0, IPIV, 1, 0 );

	var B = new Float64Array( [ 4.0, 7.0 ] );
	var info = dgbtrs( 'transpose', 2, 0, 1, 1, AB, 1, 2, 0, IPIV, 1, 0, B, 1, 2, 0 );
	assert.equal( info, 0 );
	assertClose( B[ 0 ], 2.0, 1e-14, 'x[0]' );
	assertClose( B[ 1 ], 5.0 / 3.0, 1e-14, 'x[1]' );
});

test( 'dgbtrs: KL=0 no-L path', function t() {
	// A = [2 1; 0 3], KL=0, KU=1, LDAB=2
	// b = [5, 6]
	// Solution: x2 = 6/3 = 2, x1 = (5 - 1*2)/2 = 1.5
	var AB = new Float64Array( [
		0.0, 2.0,   // col 0: [superdiag=0, diag=2]
		1.0, 3.0    // col 1: [superdiag=1, diag=3]
	] );
	var IPIV = new Int32Array( 2 );
	dgbtrf( 2, 2, 0, 1, AB, 1, 2, 0, IPIV, 1, 0 );

	var B = new Float64Array( [ 5.0, 6.0 ] );
	var info = dgbtrs( 'no-transpose', 2, 0, 1, 1, AB, 1, 2, 0, IPIV, 1, 0, B, 1, 2, 0 );
	assert.equal( info, 0 );
	assertClose( B[ 0 ], 1.5, 1e-14, 'x[0]' );
	assertClose( B[ 1 ], 2.0, 1e-14, 'x[1]' );
});
