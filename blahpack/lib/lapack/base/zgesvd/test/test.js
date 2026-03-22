'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zgesvd = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgesvd.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

function assertSingularValuesClose( actual, expected, tol, msg ) {
	var i;
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}


// TESTS //

test( 'zgesvd: m_zero (quick return)', function t() {
	var RWORK = new Float64Array( 10 );
	var WORK = new Float64Array( 100 );
	var s = new Float64Array( 1 );
	var U = new Float64Array( 2 );
	var VT = new Float64Array( 2 );
	var A = new Float64Array( 2 );
	var info;

	info = zgesvd( 'N', 'N', 0, 3, A, 1, 1, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 100, RWORK, 1, 0 );
	assert.equal( info, 0 );
});

test( 'zgesvd: n_zero (quick return)', function t() {
	var RWORK = new Float64Array( 10 );
	var WORK = new Float64Array( 100 );
	var s = new Float64Array( 1 );
	var U = new Float64Array( 2 );
	var VT = new Float64Array( 2 );
	var A = new Float64Array( 2 );
	var info;

	info = zgesvd( 'N', 'N', 3, 0, A, 1, 3, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 100, RWORK, 1, 0 );
	assert.equal( info, 0 );
});

test( 'zgesvd: full_1x1', function t() {
	var tc = findCase( 'full_1x1' );
	var RWORK = new Float64Array( 50 );
	var WORK = new Float64Array( 200 );
	var s = new Float64Array( 1 );
	var U = new Float64Array( 2 );
	var VT = new Float64Array( 2 );
	var A = new Float64Array( [ 5.0, 3.0 ] );
	var info;

	info = zgesvd( 'A', 'A', 1, 1, A, 1, 1, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 100, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: full_2x2', function t() {
	var tc = findCase( 'full_2x2' );
	var RWORK = new Float64Array( 50 );
	var WORK = new Float64Array( 1000 );
	var s = new Float64Array( 2 );
	var U = new Float64Array( 8 );
	var VT = new Float64Array( 8 );
	// Column-major 2x2: A(1,1)=3+1i, A(2,1)=1+2i, A(1,2)=2+0i, A(2,2)=4+1i
	var A = new Float64Array( [
		3.0, 1.0, 1.0, 2.0,  // column 1: (3+1i), (1+2i)
		2.0, 0.0, 4.0, 1.0   // column 2: (2+0i), (4+1i)
	] );
	var info;

	info = zgesvd( 'A', 'A', 2, 2, A, 1, 2, 0, s, 1, 0, U, 1, 2, 0, VT, 1, 2, 0, WORK, 1, 0, 500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: full_3x3', function t() {
	var tc = findCase( 'full_3x3' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 2000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 18 );
	var VT = new Float64Array( 18 );
	var A = new Float64Array( [
		1.0, 2.0, 3.0, 4.0, 5.0, 6.0,  // col 1
		7.0, 8.0, 9.0, 1.0, 2.0, 3.0,  // col 2
		4.0, 5.0, 6.0, 7.0, 8.0, 9.0   // col 3
	] );
	var info;

	info = zgesvd( 'A', 'A', 3, 3, A, 1, 3, 0, s, 1, 0, U, 1, 3, 0, VT, 1, 3, 0, WORK, 1, 0, 1000, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: values_only_3x4 (JOBU=N, JOBVT=N)', function t() {
	var tc = findCase( 'values_only_3x4' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 2000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 2 );
	var VT = new Float64Array( 2 );
	var A = new Float64Array( [
		1.0, 2.0, 3.0, 0.0, 0.0, 1.0,  // col 1
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,  // col 2
		3.0, 0.0, 1.0, 2.0, 0.0, 2.0,  // col 3
		0.0, 1.0, 2.0, 0.0, 1.0, 1.0   // col 4
	] );
	var info;

	info = zgesvd( 'N', 'N', 3, 4, A, 1, 3, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 1000, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: economy_4x3 (JOBU=S, JOBVT=S)', function t() {
	var tc = findCase( 'economy_4x3' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 5000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 24 );
	var VT = new Float64Array( 18 );
	var A = new Float64Array( [
		1.0, 0.0, 0.0, 1.0, 2.0, 1.0, 1.0, 2.0,  // col 1
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0,  // col 2
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0   // col 3
	] );
	var info;

	info = zgesvd( 'S', 'S', 4, 3, A, 1, 4, 0, s, 1, 0, U, 1, 4, 0, VT, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: full_3x5 (M < N, JOBU=A, JOBVT=A)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 5000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 18 );
	var VT = new Float64Array( 50 );
	var A = new Float64Array( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,  // col 1
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,  // col 2
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,  // col 3
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,  // col 4
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0   // col 5
	] );
	var info;

	info = zgesvd( 'A', 'A', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 3, 0, VT, 1, 5, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: economy_u_full_vt_5x3 (JOBU=S, JOBVT=A)', function t() {
	var tc = findCase( 'economy_u_full_vt_5x3' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 5000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 30 );
	var VT = new Float64Array( 18 );
	var A = new Float64Array( [
		1.0, 1.0, 2.0, 0.0, 0.0, 1.0, 3.0, 2.0, 1.0, 1.0,  // col 1
		0.0, 2.0, 1.0, 0.0, 3.0, 1.0, 2.0, 0.0, 0.0, 3.0,  // col 2
		2.0, 1.0, 0.0, 1.0, 1.0, 2.0, 1.0, 0.0, 2.0, 2.0   // col 3
	] );
	var info;

	info = zgesvd( 'S', 'A', 5, 3, A, 1, 5, 0, s, 1, 0, U, 1, 5, 0, VT, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x3 reconstruction U*S*VT ≈ A', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 2000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 18 );
	var VT = new Float64Array( 18 );
	var A_orig = new Float64Array( [
		1.0, 2.0, 3.0, 4.0, 5.0, 6.0,
		7.0, 8.0, 9.0, 1.0, 2.0, 3.0,
		4.0, 5.0, 6.0, 7.0, 8.0, 9.0
	] );
	var A = new Float64Array( A_orig );
	var info;
	var M = 3;
	var N = 3;
	var minmn = 3;
	var re;
	var im;
	var errR;
	var errI;
	var maxErr;
	var i;
	var j;
	var k;

	info = zgesvd( 'A', 'A', M, N, A, 1, M, 0, s, 1, 0, U, 1, M, 0, VT, 1, N, 0, WORK, 1, 0, 1000, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	// Reconstruct A = U * diag(S) * VT
	maxErr = 0.0;
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			re = 0.0;
			im = 0.0;
			for ( k = 0; k < minmn; k++ ) {
				// U(i,k) * s(k) * VT(k,j)
				var uR = U[ 2 * ( i + k * M ) ];
				var uI = U[ 2 * ( i + k * M ) + 1 ];
				var vtR = VT[ 2 * ( k + j * N ) ];
				var vtI = VT[ 2 * ( k + j * N ) + 1 ];
				var sk = s[ k ];
				// (uR + uI*i) * sk * (vtR + vtI*i)
				re += sk * ( uR * vtR - uI * vtI );
				im += sk * ( uR * vtI + uI * vtR );
			}
			errR = Math.abs( re - A_orig[ 2 * ( i + j * M ) ] );
			errI = Math.abs( im - A_orig[ 2 * ( i + j * M ) + 1 ] );
			maxErr = Math.max( maxErr, errR, errI );
		}
	}
	assert.ok( maxErr < 1e-10, 'reconstruction error: ' + maxErr );
});

test( 'zgesvd: 3x4 values only, M < N', function t() {
	var tc = findCase( 'values_only_3x4' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 2000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 2 );
	var VT = new Float64Array( 2 );
	// Same matrix but test with JOBU='N', JOBVT='N' (M < N path)
	var A = new Float64Array( [
		1.0, 2.0, 3.0, 0.0, 0.0, 1.0,
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		3.0, 0.0, 1.0, 2.0, 0.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 1.0
	] );
	var info;

	info = zgesvd( 'N', 'N', 3, 4, A, 1, 3, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 1000, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 4x3 JOBU=N JOBVT=N (M > N, values only)', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 2000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 2 );
	var VT = new Float64Array( 2 );
	var A = new Float64Array( [
		1.0, 0.0, 0.0, 1.0, 2.0, 1.0, 1.0, 2.0,
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0,
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0
	] );
	var info;

	// Use economy_4x3 fixture singular values for comparison
	var tc = findCase( 'economy_4x3' );
	info = zgesvd( 'N', 'N', 4, 3, A, 1, 4, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 1000, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 4x3 JOBU=A JOBVT=N (M > N, U only)', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 5000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 32 );   // 4x4 complex
	var VT = new Float64Array( 2 );
	var A = new Float64Array( [
		1.0, 0.0, 0.0, 1.0, 2.0, 1.0, 1.0, 2.0,
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0,
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0
	] );
	var info;

	var tc = findCase( 'economy_4x3' );
	info = zgesvd( 'A', 'N', 4, 3, A, 1, 4, 0, s, 1, 0, U, 1, 4, 0, VT, 1, 1, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 4x3 JOBU=N JOBVT=A (M > N, VT only)', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 5000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 2 );
	var VT = new Float64Array( 18 );   // 3x3 complex
	var A = new Float64Array( [
		1.0, 0.0, 0.0, 1.0, 2.0, 1.0, 1.0, 2.0,
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0,
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0
	] );
	var info;

	var tc = findCase( 'economy_4x3' );
	info = zgesvd( 'N', 'A', 4, 3, A, 1, 4, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x5 JOBU=N JOBVT=S (M < N, VT economy only)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 5000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 2 );
	var VT = new Float64Array( 30 );   // 3x5 complex
	var A = new Float64Array( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var info;

	info = zgesvd( 'N', 'S', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x5 JOBU=S JOBVT=N (M < N, U economy only)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 5000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 18 );   // 3x3 complex
	var VT = new Float64Array( 2 );
	var A = new Float64Array( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var info;

	info = zgesvd( 'S', 'N', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 3, 0, VT, 1, 1, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x5 JOBU=A JOBVT=N (M < N, full U only)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 5000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 18 );   // 3x3 complex
	var VT = new Float64Array( 2 );
	var A = new Float64Array( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var info;

	info = zgesvd( 'A', 'N', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 3, 0, VT, 1, 1, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x5 JOBU=N JOBVT=A (M < N, full VT only)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 5000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 2 );
	var VT = new Float64Array( 50 );   // 5x5 complex
	var A = new Float64Array( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var info;

	info = zgesvd( 'N', 'A', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 5, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 2x2 reconstruction', function t() {
	var RWORK = new Float64Array( 50 );
	var WORK = new Float64Array( 1000 );
	var s = new Float64Array( 2 );
	var U = new Float64Array( 8 );
	var VT = new Float64Array( 8 );
	var A_orig = new Float64Array( [
		3.0, 1.0, 1.0, 2.0,
		2.0, 0.0, 4.0, 1.0
	] );
	var A = new Float64Array( A_orig );
	var info;
	var M = 2;
	var N = 2;
	var re;
	var im;
	var maxErr;
	var i;
	var j;
	var k;

	info = zgesvd( 'A', 'A', M, N, A, 1, M, 0, s, 1, 0, U, 1, M, 0, VT, 1, N, 0, WORK, 1, 0, 500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	maxErr = 0.0;
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			re = 0.0;
			im = 0.0;
			for ( k = 0; k < Math.min( M, N ); k++ ) {
				var uR = U[ 2 * ( i + k * M ) ];
				var uI = U[ 2 * ( i + k * M ) + 1 ];
				var vtR = VT[ 2 * ( k + j * N ) ];
				var vtI = VT[ 2 * ( k + j * N ) + 1 ];
				var sk = s[ k ];
				re += sk * ( uR * vtR - uI * vtI );
				im += sk * ( uR * vtI + uI * vtR );
			}
			var errR = Math.abs( re - A_orig[ 2 * ( i + j * M ) ] );
			var errI = Math.abs( im - A_orig[ 2 * ( i + j * M ) + 1 ] );
			maxErr = Math.max( maxErr, errR, errI );
		}
	}
	assert.ok( maxErr < 1e-10, 'reconstruction error: ' + maxErr );
});

test( 'zgesvd: 4x3 JOBU=A JOBVT=S reconstruction', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 5000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 32 );   // 4x4 complex
	var VT = new Float64Array( 18 );  // 3x3 complex
	var M = 4;
	var N = 3;
	var A_orig = new Float64Array( [
		1.0, 0.0, 0.0, 1.0, 2.0, 1.0, 1.0, 2.0,
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0,
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0
	] );
	var A = new Float64Array( A_orig );
	var info;
	var re;
	var im;
	var maxErr;
	var i;
	var j;
	var k;
	var minmn = Math.min( M, N );

	info = zgesvd( 'A', 'S', M, N, A, 1, M, 0, s, 1, 0, U, 1, M, 0, VT, 1, minmn, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	maxErr = 0.0;
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			re = 0.0;
			im = 0.0;
			for ( k = 0; k < minmn; k++ ) {
				var uR = U[ 2 * ( i + k * M ) ];
				var uI = U[ 2 * ( i + k * M ) + 1 ];
				var vtR = VT[ 2 * ( k + j * minmn ) ];
				var vtI = VT[ 2 * ( k + j * minmn ) + 1 ];
				var sk = s[ k ];
				re += sk * ( uR * vtR - uI * vtI );
				im += sk * ( uR * vtI + uI * vtR );
			}
			var errR = Math.abs( re - A_orig[ 2 * ( i + j * M ) ] );
			var errI = Math.abs( im - A_orig[ 2 * ( i + j * M ) + 1 ] );
			maxErr = Math.max( maxErr, errR, errI );
		}
	}
	assert.ok( maxErr < 1e-10, 'reconstruction error: ' + maxErr );
});

test( 'zgesvd: 3x5 JOBU=A JOBVT=A reconstruction (M < N)', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 5000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 18 );   // 3x3 complex
	var VT = new Float64Array( 50 );  // 5x5 complex
	var M = 3;
	var N = 5;
	var A_orig = new Float64Array( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var A = new Float64Array( A_orig );
	var info;
	var re;
	var im;
	var maxErr;
	var i;
	var j;
	var k;
	var minmn = Math.min( M, N );

	info = zgesvd( 'A', 'A', M, N, A, 1, M, 0, s, 1, 0, U, 1, M, 0, VT, 1, N, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	maxErr = 0.0;
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			re = 0.0;
			im = 0.0;
			for ( k = 0; k < minmn; k++ ) {
				var uR = U[ 2 * ( i + k * M ) ];
				var uI = U[ 2 * ( i + k * M ) + 1 ];
				var vtR = VT[ 2 * ( k + j * N ) ];
				var vtI = VT[ 2 * ( k + j * N ) + 1 ];
				var sk = s[ k ];
				re += sk * ( uR * vtR - uI * vtI );
				im += sk * ( uR * vtI + uI * vtR );
			}
			var errR = Math.abs( re - A_orig[ 2 * ( i + j * M ) ] );
			var errI = Math.abs( im - A_orig[ 2 * ( i + j * M ) + 1 ] );
			maxErr = Math.max( maxErr, errR, errI );
		}
	}
	assert.ok( maxErr < 1e-10, 'reconstruction error: ' + maxErr );
});

test( 'zgesvd: 5x3 JOBU=S JOBVT=S reconstruction', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Float64Array( 5000 );
	var s = new Float64Array( 3 );
	var U = new Float64Array( 30 );   // 5x3 complex
	var VT = new Float64Array( 18 );  // 3x3 complex
	var M = 5;
	var N = 3;
	var A_orig = new Float64Array( [
		1.0, 1.0, 2.0, 0.0, 0.0, 1.0, 3.0, 2.0, 1.0, 1.0,
		0.0, 2.0, 1.0, 0.0, 3.0, 1.0, 2.0, 0.0, 0.0, 3.0,
		2.0, 1.0, 0.0, 1.0, 1.0, 2.0, 1.0, 0.0, 2.0, 2.0
	] );
	var A = new Float64Array( A_orig );
	var info;
	var re;
	var im;
	var maxErr;
	var i;
	var j;
	var k;
	var minmn = Math.min( M, N );

	info = zgesvd( 'S', 'S', M, N, A, 1, M, 0, s, 1, 0, U, 1, M, 0, VT, 1, minmn, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	maxErr = 0.0;
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			re = 0.0;
			im = 0.0;
			for ( k = 0; k < minmn; k++ ) {
				var uR = U[ 2 * ( i + k * M ) ];
				var uI = U[ 2 * ( i + k * M ) + 1 ];
				var vtR = VT[ 2 * ( k + j * minmn ) ];
				var vtI = VT[ 2 * ( k + j * minmn ) + 1 ];
				var sk = s[ k ];
				re += sk * ( uR * vtR - uI * vtI );
				im += sk * ( uR * vtI + uI * vtR );
			}
			var errR = Math.abs( re - A_orig[ 2 * ( i + j * M ) ] );
			var errI = Math.abs( im - A_orig[ 2 * ( i + j * M ) + 1 ] );
			maxErr = Math.max( maxErr, errR, errI );
		}
	}
	assert.ok( maxErr < 1e-10, 'reconstruction error: ' + maxErr );
});
