

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpbtrf = require( './../lib/ndarray.js' );

// FIXTURES //

var upper_3x3_kd1 = require( './fixtures/upper_3x3_kd1.json' );
var lower_3x3_kd1 = require( './fixtures/lower_3x3_kd1.json' );
var n_one = require( './fixtures/n_one.json' );
var not_hpd = require( './fixtures/not_hpd.json' );
var upper_blocked = require( './fixtures/upper_blocked.json' );
var lower_blocked = require( './fixtures/lower_blocked.json' );

// FUNCTIONS //

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

// TESTS //

test( 'zpbtrf: upper_3x3_kd1 (UPLO=U, N=3, KD=1, unblocked)', function t() {
	var tc = upper_3x3_kd1;
	var AB = new Complex128Array( [
		0, 0, 4, 0,
		1, 1, 5, 0,
		2, -1, 6, 0
	] );
	var info = zpbtrf( 'upper', 3, 1, AB, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( AB, 0 ) ), tc.AB, 1e-14, 'AB' );
});

test( 'zpbtrf: lower_3x3_kd1 (UPLO=L, N=3, KD=1, unblocked)', function t() {
	var tc = lower_3x3_kd1;
	var AB = new Complex128Array( [
		4, 0, 1, -1,
		5, 0, 2, 1,
		6, 0, 0, 0
	] );
	var info = zpbtrf( 'lower', 3, 1, AB, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( AB, 0 ) ), tc.AB, 1e-14, 'AB' );
});

test( 'zpbtrf: n_zero (N=0 quick return)', function t() {
	var AB = new Complex128Array( 4 );
	var info = zpbtrf( 'upper', 0, 1, AB, 1, 2, 0 );
	assert.equal( info, 0 );
});

test( 'zpbtrf: n_one (N=1)', function t() {
	var tc = n_one;
	var AB = new Complex128Array( [ 9, 0 ] );
	var info = zpbtrf( 'upper', 1, 0, AB, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( AB, 0 ) ), tc.AB, 1e-14, 'AB' );
});

test( 'zpbtrf: not_hpd (not positive definite)', function t() {
	var tc = not_hpd;
	var AB = new Complex128Array( [
		0, 0, 1, 0,
		2, 1, 1, 0
	] );
	var info = zpbtrf( 'upper', 2, 1, AB, 1, 2, 0 );
	assert.equal( info, tc.info );
});

test( 'zpbtrf: upper_blocked (UPLO=U, N=100, KD=33, blocked path)', function t() {
	var tc = upper_blocked;
	var N = 100;
	var KD = 33;
	var LDAB = KD + 1;
	var AB = new Complex128Array( LDAB * N );
	var Av = reinterpret( AB, 0 );
	var j;
	var kk;
	// Build diagonally dominant HPD banded matrix
	for ( j = 0; j < N; j++ ) {
		// Diagonal: AB[KD, j] = 100
		Av[ ( KD + j * LDAB ) * 2 ] = 100.0;
		Av[ ( KD + j * LDAB ) * 2 + 1 ] = 0.0;
		// Superdiagonals
		for ( kk = 1; kk <= KD; kk++ ) {
			if ( j + kk < N ) {
				Av[ ( KD - kk + ( j + kk ) * LDAB ) * 2 ] = 0.5 / kk;
				Av[ ( KD - kk + ( j + kk ) * LDAB ) * 2 + 1 ] = 0.1 / kk;
			}
		}
	}
	var info = zpbtrf( 'upper', N, KD, AB, 1, LDAB, 0 );
	assert.equal( info, tc.info );
	// Compare first 8 complex elements
	assertArrayClose( Array.from( Av ).slice( 0, 16 ), tc.AB, 1e-10, 'AB' );
});

test( 'zpbtrf: lower_blocked (UPLO=L, N=100, KD=33, blocked path)', function t() {
	var tc = lower_blocked;
	var N = 100;
	var KD = 33;
	var LDAB = KD + 1;
	var AB = new Complex128Array( LDAB * N );
	var Av = reinterpret( AB, 0 );
	var j;
	var kk;
	// Build diagonally dominant HPD banded matrix
	for ( j = 0; j < N; j++ ) {
		// Diagonal: AB[0, j] = 100
		Av[ ( j * LDAB ) * 2 ] = 100.0;
		Av[ ( j * LDAB ) * 2 + 1 ] = 0.0;
		// Subdiagonals
		for ( kk = 1; kk <= KD; kk++ ) {
			if ( j + kk < N ) {
				Av[ ( kk + j * LDAB ) * 2 ] = 0.5 / kk;
				Av[ ( kk + j * LDAB ) * 2 + 1 ] = -0.1 / kk;
			}
		}
	}
	var info = zpbtrf( 'lower', N, KD, AB, 1, LDAB, 0 );
	assert.equal( info, tc.info );
	// Compare first 8 complex elements
	assertArrayClose( Array.from( Av ).slice( 0, 16 ), tc.AB, 1e-10, 'AB' );
});
