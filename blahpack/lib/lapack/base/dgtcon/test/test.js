'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var dgtcon = require( './../lib/base.js' );
var dgttrf = require( '../../dgttrf/lib/base.js' );

// HELPERS //

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

/**
* Compute 1-norm of a general tridiagonal matrix.
* For column j: sum of |dl(j-1)| + |d(j)| + |du(j)| where applicable.
*/
function dlangt1( N, dlArr, dArr, duArr ) {
	var maxCol;
	var s;
	var j;

	if ( N === 0 ) {
		return 0.0;
	}
	if ( N === 1 ) {
		return Math.abs( dArr[ 0 ] );
	}

	maxCol = 0.0;
	for ( j = 0; j < N; j++ ) {
		s = Math.abs( dArr[ j ] );
		if ( j > 0 ) {
			s += Math.abs( dlArr[ j - 1 ] ); // subdiagonal
		}
		if ( j < N - 1 ) {
			s += Math.abs( duArr[ j ] ); // superdiagonal
		}
		if ( s > maxCol ) {
			maxCol = s;
		}
	}
	return maxCol;
}

/**
* Compute infinity-norm of a general tridiagonal matrix.
* Row i: |dl(i-1)| + |d(i)| + |du(i)| where applicable (max row sum).
*/
function dlangtInf( N, dlArr, dArr, duArr ) {
	var maxRow;
	var s;
	var i;

	if ( N === 0 ) {
		return 0.0;
	}
	if ( N === 1 ) {
		return Math.abs( dArr[ 0 ] );
	}

	maxRow = 0.0;
	for ( i = 0; i < N; i++ ) {
		s = Math.abs( dArr[ i ] );
		if ( i > 0 ) {
			s += Math.abs( dlArr[ i - 1 ] );
		}
		if ( i < N - 1 ) {
			s += Math.abs( duArr[ i ] );
		}
		if ( s > maxRow ) {
			maxRow = s;
		}
	}
	return maxRow;
}

/**
* Factor and compute rcond for a general tridiagonal matrix.
*/
function computeRcond( norm, dlArr, dArr, duArr ) {
	var N = dArr.length;
	var DL = new Float64Array( dlArr );
	var d = new Float64Array( dArr );
	var DU = new Float64Array( duArr );
	var DU2 = new Float64Array( Math.max( N - 2, 1 ) );
	var IPIV = new Int32Array( N );
	var work = new Float64Array( 2 * N );
	var iwork = new Int32Array( N );
	var rcond = new Float64Array( 1 );
	var anorm;
	var info;

	// Compute norm before factoring
	if ( norm === 'one-norm' ) {
		anorm = dlangt1( N, dlArr, dArr, duArr );
	} else {
		anorm = dlangtInf( N, dlArr, dArr, duArr );
	}

	// Factor
	info = dgttrf( N, DL, 1, 0, d, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	if ( info !== 0 ) {
		return { rcond: 0.0, anorm: anorm, info: info, factInfo: info };
	}

	// Compute rcond
	info = dgtcon( norm, N, DL, 1, 0, d, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0,
		anorm, rcond, work, 1, 0, iwork, 1, 0 );
	return { rcond: rcond[ 0 ], anorm: anorm, info: info };
}

// TESTS //

test( 'dgtcon: main export is a function', function t() {
	assert.strictEqual( typeof dgtcon, 'function' );
});

test( 'dgtcon: N=0 returns rcond=1 (one-norm)', function t() {
	var DL = new Float64Array( 1 );
	var d = new Float64Array( 1 );
	var DU = new Float64Array( 1 );
	var DU2 = new Float64Array( 1 );
	var IPIV = new Int32Array( 1 );
	var work = new Float64Array( 1 );
	var iwork = new Int32Array( 1 );
	var rcond = new Float64Array( 1 );
	var info = dgtcon( 'one-norm', 0, DL, 1, 0, d, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0,
		0.0, rcond, work, 1, 0, iwork, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rcond[ 0 ], 1.0 );
});

test( 'dgtcon: anorm=0 returns rcond=0', function t() {
	var DL = new Float64Array( [ 1 ] );
	var d = new Float64Array( [ 4, 4 ] );
	var DU = new Float64Array( [ 1 ] );
	var DU2 = new Float64Array( 1 );
	var IPIV = new Int32Array( 2 );
	var work = new Float64Array( 4 );
	var iwork = new Int32Array( 2 );
	var rcond = new Float64Array( 1 );
	dgttrf( 2, DL, 1, 0, d, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	var info = dgtcon( 'one-norm', 2, DL, 1, 0, d, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0,
		0.0, rcond, work, 1, 0, iwork, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rcond[ 0 ], 0.0 );
});

test( 'dgtcon: N=1 (scalar, one-norm)', function t() {
	var result = computeRcond( 'one-norm', [], [ 5.0 ], [] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, 5.0, 1e-14, 'anorm' );
	assertClose( result.rcond, 1.0, 1e-10, 'rcond' );
});

test( 'dgtcon: identity 3x3 (one-norm)', function t() {
	// dl = [0, 0], d = [1, 1, 1], du = [0, 0]
	var result = computeRcond( 'one-norm', [ 0, 0 ], [ 1, 1, 1 ], [ 0, 0 ] );
	assert.strictEqual( result.info, 0 );
	assertClose( result.anorm, 1.0, 1e-14, 'anorm' );
	assertClose( result.rcond, 1.0, 1e-10, 'rcond' );
});

test( 'dgtcon: well-conditioned 4x4 (one-norm)', function t() {
	// Diagonally dominant:
	// dl = [1, 1, 1], d = [4, 4, 4, 4], du = [1, 1, 1]
	var result = computeRcond( 'one-norm', [ 1, 1, 1 ], [ 4, 4, 4, 4 ], [ 1, 1, 1 ] );
	assert.strictEqual( result.info, 0 );
	assert.ok( result.rcond > 0.0, 'rcond should be positive' );
	assert.ok( result.rcond < 1.0, 'rcond should be less than 1' );
});

test( 'dgtcon: well-conditioned 4x4 (infinity-norm)', function t() {
	var result = computeRcond( 'infinity-norm', [ 1, 1, 1 ], [ 4, 4, 4, 4 ], [ 1, 1, 1 ] );
	assert.strictEqual( result.info, 0 );
	assert.ok( result.rcond > 0.0, 'rcond should be positive' );
	assert.ok( result.rcond < 1.0, 'rcond should be less than 1' );
});

test( 'dgtcon: asymmetric tridiagonal (one-norm)', function t() {
	// dl = [2, 3], d = [5, 6, 7], du = [1, 1]
	var result = computeRcond( 'one-norm', [ 2, 3 ], [ 5, 6, 7 ], [ 1, 1 ] );
	assert.strictEqual( result.info, 0 );
	assert.ok( result.rcond > 0.0, 'rcond should be positive' );
});

test( 'dgtcon: ill-conditioned (near-singular diagonal)', function t() {
	// d = [1, 1, 1e-15], dl = [0, 0], du = [0, 0]
	var result = computeRcond( 'one-norm', [ 0, 0 ], [ 1, 1, 1e-15 ], [ 0, 0 ] );
	assert.strictEqual( result.info, 0 );
	assert.ok( result.rcond < 1e-10, 'rcond should be very small' );
	assert.ok( result.rcond > 0.0, 'rcond should still be positive' );
});

test( 'dgtcon: singular (zero diagonal) returns rcond=0', function t() {
	// After factoring, if D has a zero, dgtcon returns rcond=0
	var DL = new Float64Array( [ 0 ] );
	var d = new Float64Array( [ 0, 1 ] );
	var DU = new Float64Array( [ 0 ] );
	var DU2 = new Float64Array( 1 );
	var IPIV = new Int32Array( 2 );
	var work = new Float64Array( 4 );
	var iwork = new Int32Array( 2 );
	var rcond = new Float64Array( 1 );
	dgttrf( 2, DL, 1, 0, d, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0 );
	var info = dgtcon( 'one-norm', 2, DL, 1, 0, d, 1, 0, DU, 1, 0, DU2, 1, 0, IPIV, 1, 0,
		1.0, rcond, work, 1, 0, iwork, 1, 0 );
	assert.strictEqual( info, 0 );
	assert.strictEqual( rcond[ 0 ], 0.0 );
});

test( 'dgtcon: 2x2 symmetric (one-norm vs infinity-norm match)', function t() {
	// For a symmetric matrix, 1-norm = infinity-norm
	var result1 = computeRcond( 'one-norm', [ 1 ], [ 3, 3 ], [ 1 ] );
	var resultI = computeRcond( 'infinity-norm', [ 1 ], [ 3, 3 ], [ 1 ] );
	assert.strictEqual( result1.info, 0 );
	assert.strictEqual( resultI.info, 0 );
	assertClose( result1.rcond, resultI.rcond, 1e-10, 'rcond should match for symmetric matrix' );
});
