'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dgetrf = require( '../../dgetrf/lib/base.js' );
var dgetri = require( './../lib/base.js' );


// FUNCTIONS //

/**
* Multiply two N-by-N column-major matrices: C = A * B.
*
* @private
* @param {NonNegativeInteger} N - matrix order
* @param {Float64Array} A - first matrix (column-major)
* @param {Float64Array} B - second matrix (column-major)
* @returns {Float64Array} C = A * B (column-major)
*/
function matmul( N, A, B ) {
	var C = new Float64Array( N * N );
	var i;
	var j;
	var k;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			for ( k = 0; k < N; k++ ) {
				C[ i + j * N ] += A[ i + k * N ] * B[ k + j * N ];
			}
		}
	}
	return C;
}

/**
* Check that a matrix is approximately the identity.
*
* @private
* @param {NonNegativeInteger} N - matrix order
* @param {Float64Array} C - matrix to check (column-major)
* @param {number} tol - tolerance
* @param {string} msg - error message prefix
*/
function assertIdentity( N, C, tol, msg ) {
	var expected;
	var err;
	var i;
	var j;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			expected = ( i === j ) ? 1.0 : 0.0;
			err = Math.abs( C[ i + j * N ] - expected );
			assert.ok( err <= tol, msg + ': C[' + i + ',' + j + '] = ' + C[ i + j * N ] + ', expected ' + expected + ', err = ' + err );
		}
	}
}


// TESTS //

test( 'dgetri: 3x3 inverse', function t() {
	// A = [2 1 1; 4 3 3; 8 7 9] (column-major)
	var Aorig = new Float64Array([ 2, 4, 8, 1, 3, 7, 1, 3, 9 ]);
	var A = new Float64Array( Aorig );
	var IPIV = new Int32Array( 3 );
	var WORK = new Float64Array( 64 );
	var info;

	// Factor
	info = dgetrf( 3, 3, A, 1, 3, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'dgetrf info' );

	// Invert
	info = dgetri( 3, A, 1, 3, 0, IPIV, 1, 0, WORK, 1, 0, 64 );
	assert.equal( info, 0, 'dgetri info' );

	// Verify: Aorig * A_inv ≈ I
	var C = matmul( 3, Aorig, A );
	assertIdentity( 3, C, 1e-13, 'A * A_inv' );
});

test( 'dgetri: 4x4 inverse', function t() {
	// A = [5 7 6 5; 7 10 8 7; 6 8 10 9; 5 7 9 10] (column-major, SPD)
	var Aorig = new Float64Array([
		5, 7, 6, 5,
		7, 10, 8, 7,
		6, 8, 10, 9,
		5, 7, 9, 10
	]);
	var A = new Float64Array( Aorig );
	var IPIV = new Int32Array( 4 );
	var WORK = new Float64Array( 128 );
	var info;

	// Factor
	info = dgetrf( 4, 4, A, 1, 4, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'dgetrf info' );

	// Invert
	info = dgetri( 4, A, 1, 4, 0, IPIV, 1, 0, WORK, 1, 0, 128 );
	assert.equal( info, 0, 'dgetri info' );

	// Verify: Aorig * A_inv ≈ I
	var C = matmul( 4, Aorig, A );
	assertIdentity( 4, C, 1e-10, 'A * A_inv' );
});

test( 'dgetri: N=1 edge case', function t() {
	var A = new Float64Array([ 4.0 ]);
	var IPIV = new Int32Array( 1 );
	var WORK = new Float64Array( 4 );
	var info;

	// Factor
	info = dgetrf( 1, 1, A, 1, 1, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'dgetrf info' );

	// Invert
	info = dgetri( 1, A, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0, 4 );
	assert.equal( info, 0, 'dgetri info' );

	// 1/4 = 0.25
	assert.ok( Math.abs( A[ 0 ] - 0.25 ) < 1e-15, 'A[0] = ' + A[ 0 ] );
});

test( 'dgetri: N=0 quick return', function t() {
	var A = new Float64Array( 1 );
	var IPIV = new Int32Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dgetri( 0, A, 1, 1, 0, IPIV, 1, 0, WORK, 1, 0, 1 );
	assert.equal( info, 0, 'info should be 0' );
});

test( 'dgetri: 3x3 different pivots', function t() {
	// A = [1 2 3; 4 5 6; 7 8 0] (column-major)
	var Aorig = new Float64Array([ 1, 4, 7, 2, 5, 8, 3, 6, 0 ]);
	var A = new Float64Array( Aorig );
	var IPIV = new Int32Array( 3 );
	var WORK = new Float64Array( 64 );
	var info;

	// Factor
	info = dgetrf( 3, 3, A, 1, 3, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'dgetrf info' );

	// Invert
	info = dgetri( 3, A, 1, 3, 0, IPIV, 1, 0, WORK, 1, 0, 64 );
	assert.equal( info, 0, 'dgetri info' );

	// Verify: Aorig * A_inv ≈ I
	var C = matmul( 3, Aorig, A );
	assertIdentity( 3, C, 1e-13, 'A * A_inv' );
});

test( 'dgetri: singular matrix returns info > 0', function t() {
	// A = [1 2; 2 4] (singular, column-major)
	var A = new Float64Array([ 1, 2, 2, 4 ]);
	var IPIV = new Int32Array( 2 );
	var WORK = new Float64Array( 16 );
	var info;

	// Factor - should detect singularity
	info = dgetrf( 2, 2, A, 1, 2, 0, IPIV, 1, 0 );
	assert.ok( info > 0, 'dgetrf should detect singular matrix, info=' + info );

	// Invert - should also return info > 0 (singular U factor)
	info = dgetri( 2, A, 1, 2, 0, IPIV, 1, 0, WORK, 1, 0, 16 );
	assert.ok( info > 0, 'dgetri should return info > 0 for singular matrix, info=' + info );
});

test( 'dgetri: 5x5 matrix inverse', function t() {
	// Diagonally dominant 5x5 matrix for good conditioning
	var Aorig = new Float64Array([
		10, 1, 2, 0, 1,
		1, 10, 1, 2, 0,
		2, 1, 10, 1, 2,
		0, 2, 1, 10, 1,
		1, 0, 2, 1, 10
	]);
	var A = new Float64Array( Aorig );
	var IPIV = new Int32Array( 5 );
	var WORK = new Float64Array( 256 );
	var info;

	// Factor
	info = dgetrf( 5, 5, A, 1, 5, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'dgetrf info' );

	// Invert
	info = dgetri( 5, A, 1, 5, 0, IPIV, 1, 0, WORK, 1, 0, 256 );
	assert.equal( info, 0, 'dgetri info' );

	// Verify: Aorig * A_inv ≈ I
	var C = matmul( 5, Aorig, A );
	assertIdentity( 5, C, 1e-12, 'A * A_inv' );
});

test( 'dgetri: blocked path (large matrix)', function t() {
	// 35x35 diagonally dominant matrix to exercise the blocked path (NB=32, N=35 > NB)
	var N = 35;
	var Aorig = new Float64Array( N * N );
	var i;
	var j;

	// Create diagonally dominant matrix
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			if ( i === j ) {
				Aorig[ i + j * N ] = N + 1.0;
			} else {
				Aorig[ i + j * N ] = 1.0 / ( 1.0 + Math.abs( i - j ) );
			}
		}
	}

	var A = new Float64Array( Aorig );
	var IPIV = new Int32Array( N );
	var WORK = new Float64Array( N * 64 );
	var info;

	// Factor
	info = dgetrf( N, N, A, 1, N, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'dgetrf info' );

	// Invert with enough workspace for blocked algorithm
	info = dgetri( N, A, 1, N, 0, IPIV, 1, 0, WORK, 1, 0, N * 64 );
	assert.equal( info, 0, 'dgetri info' );

	// Verify: Aorig * A_inv ≈ I
	var C = matmul( N, Aorig, A );
	assertIdentity( N, C, 1e-10, 'A * A_inv' );
});

test( 'dgetri: blocked path with insufficient workspace (lwork < N*NB)', function t() {
	// N=35 > NB=32, but lwork < N*NB to trigger nb = floor(lwork/ldwork) adjustment
	var N = 35;
	var Aorig = new Float64Array( N * N );
	var i;
	var j;

	// Create diagonally dominant matrix
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < N; i++ ) {
			if ( i === j ) {
				Aorig[ i + j * N ] = N + 1.0;
			} else {
				Aorig[ i + j * N ] = 1.0 / ( 1.0 + Math.abs( i - j ) );
			}
		}
	}

	var A = new Float64Array( Aorig );
	var IPIV = new Int32Array( N );
	// Provide workspace smaller than N*NB (35*32=1120) but >= N*2 (70)
	// This forces nb = floor(lwork/N) = floor(105/35) = 3
	var lwork = 105;
	var WORK = new Float64Array( lwork );
	var info;

	// Factor
	info = dgetrf( N, N, A, 1, N, 0, IPIV, 1, 0 );
	assert.equal( info, 0, 'dgetrf info' );

	// Invert with insufficient workspace
	info = dgetri( N, A, 1, N, 0, IPIV, 1, 0, WORK, 1, 0, lwork );
	assert.equal( info, 0, 'dgetri info' );

	// Verify: Aorig * A_inv ≈ I
	var C = matmul( N, Aorig, A );
	assertIdentity( N, C, 1e-10, 'A * A_inv' );
});
