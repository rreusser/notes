'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dsymv = require( './../../../../blas/base/dsymv/lib/base.js' );
var dsytrf = require( './../../dsytrf/lib/base.js' );
var dsytrs = require( './../../dsytrs/lib/base.js' );
var dsyrfs = require( './../lib/base.js' );


// FUNCTIONS //

/**
* Compute ||A*x - b||_inf / (||A||_inf * ||x||_inf * N * eps).
* Returns the backward error ratio.
*/
function residualCheck( uplo, N, A, x, b ) {
	var r = new Float64Array( N );
	var maxR;
	var normX;
	var i;

	// r = b
	for ( i = 0; i < N; i++ ) {
		r[ i ] = b[ i ];
	}
	// r = A*x - b = -1*A*x + 1*r => r = b - A*x
	dsymv( uplo, N, -1.0, A, 1, N, 0, x, 1, 0, 1.0, r, 1, 0 );

	maxR = 0.0;
	normX = 0.0;
	for ( i = 0; i < N; i++ ) {
		maxR = Math.max( maxR, Math.abs( r[ i ] ) );
		normX = Math.max( normX, Math.abs( x[ i ] ) );
	}
	return maxR / ( normX || 1.0 );
}

/**
* Helper: factorize symmetric matrix, solve, then refine.
*
* @param {string} uplo - 'upper' or 'lower'
* @param {number} N - matrix order
* @param {number} nrhs - number of right-hand sides
* @param {Float64Array} A - original symmetric matrix (column-major, N*N)
* @param {Float64Array} b - right-hand side matrix (column-major, N*nrhs)
* @returns {Object} result with info, x, ferr, berr
*/
function solveAndRefine( uplo, N, nrhs, A, b ) {
	var FERR = new Float64Array( nrhs );
	var BERR = new Float64Array( nrhs );
	var WORK = new Float64Array( 3 * N );
	var IWORK = new Int32Array( N );
	var AF = new Float64Array( N * N );
	var X = new Float64Array( N * nrhs );
	var IPIV = new Int32Array( N );
	var info;
	var i;

	for ( i = 0; i < N * N; i++ ) {
		AF[ i ] = A[ i ];
	}
	for ( i = 0; i < N * nrhs; i++ ) {
		X[ i ] = b[ i ];
	}

	info = dsytrf( uplo, N, AF, 1, N, 0, IPIV, 1, 0 );
	if ( info !== 0 ) {
		return { info: info, x: X, ferr: FERR, berr: BERR, factInfo: info };
	}

	dsytrs( uplo, N, nrhs, AF, 1, N, 0, IPIV, 1, 0, X, 1, N, 0 );
	info = dsyrfs( uplo, N, nrhs, A, 1, N, 0, AF, 1, N, 0, IPIV, 1, 0, b, 1, N, 0, X, 1, N, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );

	return { info: info, x: X, ferr: FERR, berr: BERR };
}


// TESTS //

test( 'dsyrfs: upper_3x3 - single RHS', function t() {
	// A = [4 2 1; 2 5 3; 1 3 6] stored as upper triangle, column-major
	var A = new Float64Array([
		4.0, 0.0, 0.0,
		2.0, 5.0, 0.0,
		1.0, 3.0, 6.0
	]);
	var b = new Float64Array([ 1.0, 2.0, 3.0 ]);
	var result = solveAndRefine( 'upper', 3, 1, A, b );
	var resid;

	assert.equal( result.info, 0 );

	// Verify A*x = b to high accuracy
	resid = residualCheck( 'upper', 3, A, result.x, b );
	assert.ok( resid < 1e-14, 'residual should be near machine epsilon, got ' + resid );

	// BERR and FERR should be very small
	assert.ok( result.berr[ 0 ] < 1e-13, 'berr should be tiny, got ' + result.berr[ 0 ] );
	assert.ok( result.ferr[ 0 ] < 1e-12, 'ferr should be small, got ' + result.ferr[ 0 ] );
});

test( 'dsyrfs: lower_3x3 - single RHS', function t() {
	// Same matrix stored as lower triangle
	var A = new Float64Array([
		4.0, 2.0, 1.0,
		0.0, 5.0, 3.0,
		0.0, 0.0, 6.0
	]);
	var b = new Float64Array([ 1.0, 2.0, 3.0 ]);
	var result = solveAndRefine( 'lower', 3, 1, A, b );
	var resid;

	assert.equal( result.info, 0 );

	resid = residualCheck( 'lower', 3, A, result.x, b );
	assert.ok( resid < 1e-14, 'residual should be near machine epsilon, got ' + resid );

	assert.ok( result.berr[ 0 ] < 1e-13, 'berr should be tiny, got ' + result.berr[ 0 ] );
	assert.ok( result.ferr[ 0 ] < 1e-12, 'ferr should be small, got ' + result.ferr[ 0 ] );
});

test( 'dsyrfs: multi_rhs - two right-hand sides', function t() {
	var A = new Float64Array([
		4.0, 0.0, 0.0,
		2.0, 5.0, 0.0,
		1.0, 3.0, 6.0
	]);
	// b1 = [1;0;0], b2 = [0;1;0] stored column-major
	var b = new Float64Array([ 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 ]);
	var result = solveAndRefine( 'upper', 3, 2, A, b );
	var x1 = new Float64Array( result.x.buffer, 0, 3 );
	var x2 = new Float64Array( result.x.buffer, 3 * 8, 3 );
	var b1 = new Float64Array([ 1.0, 0.0, 0.0 ]);
	var b2 = new Float64Array([ 0.0, 1.0, 0.0 ]);
	var resid1;
	var resid2;

	assert.equal( result.info, 0 );

	resid1 = residualCheck( 'upper', 3, A, x1, b1 );
	resid2 = residualCheck( 'upper', 3, A, x2, b2 );
	assert.ok( resid1 < 1e-14, 'residual for RHS 1: ' + resid1 );
	assert.ok( resid2 < 1e-14, 'residual for RHS 2: ' + resid2 );

	assert.ok( result.berr[ 0 ] < 1e-13, 'berr[0]: ' + result.berr[ 0 ] );
	assert.ok( result.berr[ 1 ] < 1e-13, 'berr[1]: ' + result.berr[ 1 ] );
});

test( 'dsyrfs: N=0 quick return', function t() {
	var FERR = new Float64Array([ 999.0 ]);
	var BERR = new Float64Array([ 999.0 ]);
	var WORK = new Float64Array( 1 );
	var IWORK = new Int32Array( 1 );
	var A = new Float64Array( 1 );
	var AF = new Float64Array( 1 );
	var IPIV = new Int32Array( 1 );
	var B = new Float64Array( 1 );
	var X = new Float64Array( 1 );
	var info;

	info = dsyrfs( 'upper', 0, 1, A, 1, 1, 0, AF, 1, 1, 0, IPIV, 1, 0, B, 1, 1, 0, X, 1, 1, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
	assert.equal( info, 0 );
	assert.equal( FERR[ 0 ], 0.0, 'FERR should be zero for N=0' );
	assert.equal( BERR[ 0 ], 0.0, 'BERR should be zero for N=0' );
});

test( 'dsyrfs: NRHS=0 quick return', function t() {
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var WORK = new Float64Array( 9 );
	var IWORK = new Int32Array( 3 );
	var A = new Float64Array( 9 );
	var AF = new Float64Array( 9 );
	var IPIV = new Int32Array( 3 );
	var B = new Float64Array( 3 );
	var X = new Float64Array( 3 );
	var info;

	info = dsyrfs( 'upper', 3, 0, A, 1, 3, 0, AF, 1, 3, 0, IPIV, 1, 0, B, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );
	assert.equal( info, 0 );
});

test( 'dsyrfs: 1x1 system', function t() {
	var A = new Float64Array([ 5.0 ]);
	var b = new Float64Array([ 10.0 ]);
	var result = solveAndRefine( 'upper', 1, 1, A, b );

	assert.equal( result.info, 0 );
	assert.ok( Math.abs( result.x[ 0 ] - 2.0 ) < 1e-14, 'x should be 2.0, got ' + result.x[ 0 ] );
	assert.ok( result.berr[ 0 ] <= 1e-14, 'berr should be ~0' );
});

test( 'dsyrfs: ill-conditioned Hilbert upper', function t() {
	// Hilbert matrix upper triangle, column-major
	var A = new Float64Array([
		1.0, 0.0, 0.0,
		0.5, 1.0 / 3.0, 0.0,
		1.0 / 3.0, 0.25, 0.2
	]);
	var b = new Float64Array([ 1.0, 1.0, 1.0 ]);
	var result = solveAndRefine( 'upper', 3, 1, A, b );
	var resid;

	assert.equal( result.info, 0 );

	resid = residualCheck( 'upper', 3, A, result.x, b );
	assert.ok( resid < 1e-10, 'residual for Hilbert matrix: ' + resid );
	assert.ok( result.ferr[ 0 ] < 1e-8, 'ferr should reflect ill-conditioning, got ' + result.ferr[ 0 ] );
});

test( 'dsyrfs: lower with non-trivial pivoting (indefinite matrix)', function t() {
	// Indefinite symmetric matrix to exercise pivoting
	// A = [1 2 3; 2 -1 4; 3 4 0], stored lower
	var A = new Float64Array([
		1.0, 2.0, 3.0,
		0.0, -1.0, 4.0,
		0.0, 0.0, 0.0
	]);
	var b = new Float64Array([ 6.0, 5.0, 7.0 ]);
	var result = solveAndRefine( 'lower', 3, 1, A, b );
	var resid;

	assert.equal( result.info, 0 );

	resid = residualCheck( 'lower', 3, A, result.x, b );
	assert.ok( resid < 1e-13, 'residual for indefinite matrix: ' + resid );
	assert.ok( result.berr[ 0 ] < 1e-13, 'berr: ' + result.berr[ 0 ] );
});

test( 'dsyrfs: perturbed solution triggers iterative refinement', function t() {
	// Deliberately perturb the initial solve to trigger the refinement loop
	var A = new Float64Array([
		4.0, 0.0, 0.0,
		2.0, 5.0, 0.0,
		1.0, 3.0, 6.0
	]);
	var b = new Float64Array([ 1.0, 2.0, 3.0 ]);
	var FERR = new Float64Array( 1 );
	var BERR = new Float64Array( 1 );
	var WORK = new Float64Array( 9 );
	var IWORK = new Int32Array( 3 );
	var AF = new Float64Array( 9 );
	var X = new Float64Array( 3 );
	var IPIV = new Int32Array( 3 );
	var info;
	var resid;
	var i;

	for ( i = 0; i < 9; i++ ) {
		AF[ i ] = A[ i ];
	}

	// Factorize
	info = dsytrf( 'upper', 3, AF, 1, 3, 0, IPIV, 1, 0 );
	assert.equal( info, 0 );

	// Solve
	X[ 0 ] = b[ 0 ]; X[ 1 ] = b[ 1 ]; X[ 2 ] = b[ 2 ];
	dsytrs( 'upper', 3, 1, AF, 1, 3, 0, IPIV, 1, 0, X, 1, 3, 0 );

	// Perturb the solution to trigger iterative refinement
	X[ 0 ] += 1e-8;
	X[ 1 ] -= 1e-8;
	X[ 2 ] += 1e-8;

	// Refine
	info = dsyrfs( 'upper', 3, 1, A, 1, 3, 0, AF, 1, 3, 0, IPIV, 1, 0, b, 1, 3, 0, X, 1, 3, 0, FERR, 1, 0, BERR, 1, 0, WORK, 1, 0, IWORK, 1, 0 );

	assert.equal( info, 0 );
	resid = residualCheck( 'upper', 3, A, X, b );
	assert.ok( resid < 1e-14, 'refinement should recover accurate solution, residual: ' + resid );
});
