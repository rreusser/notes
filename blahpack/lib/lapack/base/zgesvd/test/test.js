'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
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

function c128( arr ) {
	return new Complex128Array( new Float64Array( arr ).buffer );
}


// TESTS //

test( 'zgesvd: m_zero (quick return)', function t() {
	var RWORK = new Float64Array( 10 );
	var WORK = new Complex128Array( 50 );
	var s = new Float64Array( 1 );
	var U = new Complex128Array( 1 );
	var VT = new Complex128Array( 1 );
	var A = new Complex128Array( 1 );
	var info;

	info = zgesvd( 'none', 'none', 0, 3, A, 1, 1, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 50, RWORK, 1, 0 );
	assert.equal( info, 0 );
});

test( 'zgesvd: n_zero (quick return)', function t() {
	var RWORK = new Float64Array( 10 );
	var WORK = new Complex128Array( 50 );
	var s = new Float64Array( 1 );
	var U = new Complex128Array( 1 );
	var VT = new Complex128Array( 1 );
	var A = new Complex128Array( 1 );
	var info;

	info = zgesvd( 'none', 'none', 3, 0, A, 1, 3, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 50, RWORK, 1, 0 );
	assert.equal( info, 0 );
});

test( 'zgesvd: full_1x1', function t() {
	var tc = findCase( 'full_1x1' );
	var RWORK = new Float64Array( 50 );
	var WORK = new Complex128Array( 100 );
	var s = new Float64Array( 1 );
	var U = new Complex128Array( 1 );
	var VT = new Complex128Array( 1 );
	var A = c128( [ 5.0, 3.0 ] );
	var info;

	info = zgesvd( 'all-columns', 'all-rows', 1, 1, A, 1, 1, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 100, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: full_2x2', function t() {
	var tc = findCase( 'full_2x2' );
	var RWORK = new Float64Array( 50 );
	var WORK = new Complex128Array( 500 );
	var s = new Float64Array( 2 );
	var U = new Complex128Array( 4 );
	var VT = new Complex128Array( 4 );
	// Column-major 2x2: A(1,1)=3+1i, A(2,1)=1+2i, A(1,2)=2+0i, A(2,2)=4+1i
	var A = c128( [
		3.0, 1.0, 1.0, 2.0,  // column 1: (3+1i), (1+2i)
		2.0, 0.0, 4.0, 1.0   // column 2: (2+0i), (4+1i)
	] );
	var info;

	info = zgesvd( 'all-columns', 'all-rows', 2, 2, A, 1, 2, 0, s, 1, 0, U, 1, 2, 0, VT, 1, 2, 0, WORK, 1, 0, 500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: full_3x3', function t() {
	var tc = findCase( 'full_3x3' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 1000 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 9 );
	var VT = new Complex128Array( 9 );
	var A = c128( [
		1.0, 2.0, 3.0, 4.0, 5.0, 6.0,  // col 1
		7.0, 8.0, 9.0, 1.0, 2.0, 3.0,  // col 2
		4.0, 5.0, 6.0, 7.0, 8.0, 9.0   // col 3
	] );
	var info;

	info = zgesvd( 'all-columns', 'all-rows', 3, 3, A, 1, 3, 0, s, 1, 0, U, 1, 3, 0, VT, 1, 3, 0, WORK, 1, 0, 1000, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: values_only_3x4 (JOBU=N, JOBVT=N)', function t() {
	var tc = findCase( 'values_only_3x4' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 1000 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 1 );
	var VT = new Complex128Array( 1 );
	var A = c128( [
		1.0, 2.0, 3.0, 0.0, 0.0, 1.0,  // col 1
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,  // col 2
		3.0, 0.0, 1.0, 2.0, 0.0, 2.0,  // col 3
		0.0, 1.0, 2.0, 0.0, 1.0, 1.0   // col 4
	] );
	var info;

	info = zgesvd( 'none', 'none', 3, 4, A, 1, 3, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 1000, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: economy_4x3 (JOBU=S, JOBVT=S)', function t() {
	var tc = findCase( 'economy_4x3' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 12 );
	var VT = new Complex128Array( 9 );
	var A = c128( [
		1.0, 0.0, 0.0, 1.0, 2.0, 1.0, 1.0, 2.0,  // col 1
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0,  // col 2
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0   // col 3
	] );
	var info;

	info = zgesvd( 'economy', 'economy', 4, 3, A, 1, 4, 0, s, 1, 0, U, 1, 4, 0, VT, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: full_3x5 (M < N, JOBU=A, JOBVT=A)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 9 );
	var VT = new Complex128Array( 25 );
	var A = c128( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,  // col 1
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,  // col 2
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,  // col 3
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,  // col 4
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0   // col 5
	] );
	var info;

	info = zgesvd( 'all-columns', 'all-rows', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 3, 0, VT, 1, 5, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: economy_u_full_vt_5x3 (JOBU=S, JOBVT=A)', function t() {
	var tc = findCase( 'economy_u_full_vt_5x3' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 15 );
	var VT = new Complex128Array( 9 );
	var A = c128( [
		1.0, 1.0, 2.0, 0.0, 0.0, 1.0, 3.0, 2.0, 1.0, 1.0,  // col 1
		0.0, 2.0, 1.0, 0.0, 3.0, 1.0, 2.0, 0.0, 0.0, 3.0,  // col 2
		2.0, 1.0, 0.0, 1.0, 1.0, 2.0, 1.0, 0.0, 2.0, 2.0   // col 3
	] );
	var info;

	info = zgesvd( 'economy', 'all-rows', 5, 3, A, 1, 5, 0, s, 1, 0, U, 1, 5, 0, VT, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x3 reconstruction U*S*VT ~ A', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 1000 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 9 );
	var VT = new Complex128Array( 9 );
	var A_orig_f64 = new Float64Array( [
		1.0, 2.0, 3.0, 4.0, 5.0, 6.0,
		7.0, 8.0, 9.0, 1.0, 2.0, 3.0,
		4.0, 5.0, 6.0, 7.0, 8.0, 9.0
	] );
	var A = c128( Array.from( A_orig_f64 ) );
	var Uv;
	var VTv;
	var info;
	var M = 3;
	var N = 3;
	var minmn = 3;
	var re;
	var im;
	var errR;
	var errI;
	var maxErr;
	var uR;
	var uI;
	var vtR;
	var vtI;
	var sk;
	var i;
	var j;
	var k;

	info = zgesvd( 'all-columns', 'all-rows', M, N, A, 1, M, 0, s, 1, 0, U, 1, M, 0, VT, 1, N, 0, WORK, 1, 0, 1000, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	Uv = reinterpret( U, 0 );
	VTv = reinterpret( VT, 0 );

	// Reconstruct A = U * diag(S) * VT
	maxErr = 0.0;
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			re = 0.0;
			im = 0.0;
			for ( k = 0; k < minmn; k++ ) {
				// U(i,k) * s(k) * VT(k,j)
				uR = Uv[ 2 * ( i + k * M ) ];
				uI = Uv[ 2 * ( i + k * M ) + 1 ];
				vtR = VTv[ 2 * ( k + j * N ) ];
				vtI = VTv[ 2 * ( k + j * N ) + 1 ];
				sk = s[ k ];
				// (uR + uI*i) * sk * (vtR + vtI*i)
				re += sk * ( uR * vtR - uI * vtI );
				im += sk * ( uR * vtI + uI * vtR );
			}
			errR = Math.abs( re - A_orig_f64[ 2 * ( i + j * M ) ] );
			errI = Math.abs( im - A_orig_f64[ 2 * ( i + j * M ) + 1 ] );
			maxErr = Math.max( maxErr, errR, errI );
		}
	}
	assert.ok( maxErr < 1e-10, 'reconstruction error: ' + maxErr );
});

test( 'zgesvd: 3x4 values only, M < N', function t() {
	var tc = findCase( 'values_only_3x4' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 1000 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 1 );
	var VT = new Complex128Array( 1 );
	// Same matrix but test with JOBU='N', JOBVT='N' (M < N path)
	var A = c128( [
		1.0, 2.0, 3.0, 0.0, 0.0, 1.0,
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		3.0, 0.0, 1.0, 2.0, 0.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 1.0
	] );
	var info;

	info = zgesvd( 'none', 'none', 3, 4, A, 1, 3, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 1000, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 4x3 JOBU=N JOBVT=N (M > N, values only)', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 1000 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 1 );
	var VT = new Complex128Array( 1 );
	var A = c128( [
		1.0, 0.0, 0.0, 1.0, 2.0, 1.0, 1.0, 2.0,
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0,
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0
	] );
	var info;

	// Use economy_4x3 fixture singular values for comparison
	var tc = findCase( 'economy_4x3' );
	info = zgesvd( 'none', 'none', 4, 3, A, 1, 4, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 1000, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 4x3 JOBU=A JOBVT=N (M > N, U only)', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 16 );   // 4x4 complex
	var VT = new Complex128Array( 1 );
	var A = c128( [
		1.0, 0.0, 0.0, 1.0, 2.0, 1.0, 1.0, 2.0,
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0,
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0
	] );
	var info;

	var tc = findCase( 'economy_4x3' );
	info = zgesvd( 'all-columns', 'none', 4, 3, A, 1, 4, 0, s, 1, 0, U, 1, 4, 0, VT, 1, 1, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 4x3 JOBU=N JOBVT=A (M > N, VT only)', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 1 );
	var VT = new Complex128Array( 9 );   // 3x3 complex
	var A = c128( [
		1.0, 0.0, 0.0, 1.0, 2.0, 1.0, 1.0, 2.0,
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0,
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0
	] );
	var info;

	var tc = findCase( 'economy_4x3' );
	info = zgesvd( 'none', 'all-rows', 4, 3, A, 1, 4, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x5 JOBU=N JOBVT=S (M < N, VT economy only)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 1 );
	var VT = new Complex128Array( 15 );   // 3x5 complex
	var A = c128( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var info;

	info = zgesvd( 'none', 'economy', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x5 JOBU=S JOBVT=N (M < N, U economy only)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 9 );   // 3x3 complex
	var VT = new Complex128Array( 1 );
	var A = c128( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var info;

	info = zgesvd( 'economy', 'none', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 3, 0, VT, 1, 1, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x5 JOBU=A JOBVT=N (M < N, full U only)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 9 );   // 3x3 complex
	var VT = new Complex128Array( 1 );
	var A = c128( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var info;

	info = zgesvd( 'all-columns', 'none', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 3, 0, VT, 1, 1, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x5 JOBU=N JOBVT=A (M < N, full VT only)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 1 );
	var VT = new Complex128Array( 25 );   // 5x5 complex
	var A = c128( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var info;

	info = zgesvd( 'none', 'all-rows', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 5, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 2x2 reconstruction', function t() {
	var RWORK = new Float64Array( 50 );
	var WORK = new Complex128Array( 500 );
	var s = new Float64Array( 2 );
	var U = new Complex128Array( 4 );
	var VT = new Complex128Array( 4 );
	var A_orig_f64 = new Float64Array( [
		3.0, 1.0, 1.0, 2.0,
		2.0, 0.0, 4.0, 1.0
	] );
	var A = c128( Array.from( A_orig_f64 ) );
	var Uv;
	var VTv;
	var info;
	var M = 2;
	var N = 2;
	var re;
	var im;
	var maxErr;
	var uR;
	var uI;
	var vtR;
	var vtI;
	var sk;
	var errR;
	var errI;
	var i;
	var j;
	var k;

	info = zgesvd( 'all-columns', 'all-rows', M, N, A, 1, M, 0, s, 1, 0, U, 1, M, 0, VT, 1, N, 0, WORK, 1, 0, 500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	Uv = reinterpret( U, 0 );
	VTv = reinterpret( VT, 0 );

	maxErr = 0.0;
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			re = 0.0;
			im = 0.0;
			for ( k = 0; k < Math.min( M, N ); k++ ) {
				uR = Uv[ 2 * ( i + k * M ) ];
				uI = Uv[ 2 * ( i + k * M ) + 1 ];
				vtR = VTv[ 2 * ( k + j * N ) ];
				vtI = VTv[ 2 * ( k + j * N ) + 1 ];
				sk = s[ k ];
				re += sk * ( uR * vtR - uI * vtI );
				im += sk * ( uR * vtI + uI * vtR );
			}
			errR = Math.abs( re - A_orig_f64[ 2 * ( i + j * M ) ] );
			errI = Math.abs( im - A_orig_f64[ 2 * ( i + j * M ) + 1 ] );
			maxErr = Math.max( maxErr, errR, errI );
		}
	}
	assert.ok( maxErr < 1e-10, 'reconstruction error: ' + maxErr );
});

test( 'zgesvd: 4x3 JOBU=A JOBVT=S reconstruction', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 16 );   // 4x4 complex
	var VT = new Complex128Array( 9 );  // 3x3 complex
	var M = 4;
	var N = 3;
	var A_orig_f64 = new Float64Array( [
		1.0, 0.0, 0.0, 1.0, 2.0, 1.0, 1.0, 2.0,
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0,
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0
	] );
	var A = c128( Array.from( A_orig_f64 ) );
	var Uv;
	var VTv;
	var info;
	var re;
	var im;
	var maxErr;
	var uR;
	var uI;
	var vtR;
	var vtI;
	var sk;
	var errR;
	var errI;
	var i;
	var j;
	var k;
	var minmn = Math.min( M, N );

	info = zgesvd( 'all-columns', 'economy', M, N, A, 1, M, 0, s, 1, 0, U, 1, M, 0, VT, 1, minmn, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	Uv = reinterpret( U, 0 );
	VTv = reinterpret( VT, 0 );

	maxErr = 0.0;
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			re = 0.0;
			im = 0.0;
			for ( k = 0; k < minmn; k++ ) {
				uR = Uv[ 2 * ( i + k * M ) ];
				uI = Uv[ 2 * ( i + k * M ) + 1 ];
				vtR = VTv[ 2 * ( k + j * minmn ) ];
				vtI = VTv[ 2 * ( k + j * minmn ) + 1 ];
				sk = s[ k ];
				re += sk * ( uR * vtR - uI * vtI );
				im += sk * ( uR * vtI + uI * vtR );
			}
			errR = Math.abs( re - A_orig_f64[ 2 * ( i + j * M ) ] );
			errI = Math.abs( im - A_orig_f64[ 2 * ( i + j * M ) + 1 ] );
			maxErr = Math.max( maxErr, errR, errI );
		}
	}
	assert.ok( maxErr < 1e-10, 'reconstruction error: ' + maxErr );
});

test( 'zgesvd: 3x5 JOBU=A JOBVT=A reconstruction (M < N)', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 9 );   // 3x3 complex
	var VT = new Complex128Array( 25 );  // 5x5 complex
	var M = 3;
	var N = 5;
	var A_orig_f64 = new Float64Array( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var A = c128( Array.from( A_orig_f64 ) );
	var Uv;
	var VTv;
	var info;
	var re;
	var im;
	var maxErr;
	var uR;
	var uI;
	var vtR;
	var vtI;
	var sk;
	var errR;
	var errI;
	var i;
	var j;
	var k;
	var minmn = Math.min( M, N );

	info = zgesvd( 'all-columns', 'all-rows', M, N, A, 1, M, 0, s, 1, 0, U, 1, M, 0, VT, 1, N, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	Uv = reinterpret( U, 0 );
	VTv = reinterpret( VT, 0 );

	maxErr = 0.0;
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			re = 0.0;
			im = 0.0;
			for ( k = 0; k < minmn; k++ ) {
				uR = Uv[ 2 * ( i + k * M ) ];
				uI = Uv[ 2 * ( i + k * M ) + 1 ];
				vtR = VTv[ 2 * ( k + j * N ) ];
				vtI = VTv[ 2 * ( k + j * N ) + 1 ];
				sk = s[ k ];
				re += sk * ( uR * vtR - uI * vtI );
				im += sk * ( uR * vtI + uI * vtR );
			}
			errR = Math.abs( re - A_orig_f64[ 2 * ( i + j * M ) ] );
			errI = Math.abs( im - A_orig_f64[ 2 * ( i + j * M ) + 1 ] );
			maxErr = Math.max( maxErr, errR, errI );
		}
	}
	assert.ok( maxErr < 1e-10, 'reconstruction error: ' + maxErr );
});

test( 'zgesvd: 6x3 JOBU=N JOBVT=N (M >= 2N path 1, values only)', function t() {
	// M=6, N=3 => M >= 2*N path (wntun && M >= 2*N)
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var sRef = new Float64Array( 3 );
	var U = new Complex128Array( 1 );
	var VT = new Complex128Array( 1 );
	var A_orig_f64 = new Float64Array( [
		1.0, 0.0, 2.0, 1.0, 0.0, 1.0, 3.0, 2.0, 1.0, 1.0, 0.0, 2.0,
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0, 1.0, 1.0, 2.0, 1.0,
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0, 3.0, 0.0, 1.0, 2.0
	] );
	var A = c128( Array.from( A_orig_f64 ) );
	var Aref = c128( Array.from( A_orig_f64 ) );
	var info;
	var infoRef;

	// Compute reference with full SVD
	var Uref = new Complex128Array( 36 );
	var VTref = new Complex128Array( 9 );
	infoRef = zgesvd( 'all-columns', 'all-rows', 6, 3, Aref, 1, 6, 0, sRef, 1, 0, Uref, 1, 6, 0, VTref, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( infoRef, 0, 'ref info' );

	// Now test the N path (triggers path 1: wntun && M >= 2*N)
	info = zgesvd( 'none', 'none', 6, 3, A, 1, 6, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), Array.from( sRef ), 1e-12, 's' );
});

test( 'zgesvd: 6x3 JOBU=N JOBVT=S (M >= 2N path 1, VT economy)', function t() {
	// M=6, N=3 => M >= 2*N path, with VT requested
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var sRef = new Float64Array( 3 );
	var U = new Complex128Array( 1 );
	var VT = new Complex128Array( 9 );   // 3x3
	var A_orig_f64 = new Float64Array( [
		1.0, 0.0, 2.0, 1.0, 0.0, 1.0, 3.0, 2.0, 1.0, 1.0, 0.0, 2.0,
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0, 1.0, 1.0, 2.0, 1.0,
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0, 3.0, 0.0, 1.0, 2.0
	] );
	var A = c128( Array.from( A_orig_f64 ) );
	var Aref = c128( Array.from( A_orig_f64 ) );
	var info;

	// Compute reference
	var Uref = new Complex128Array( 36 );
	var VTref = new Complex128Array( 9 );
	zgesvd( 'all-columns', 'all-rows', 6, 3, Aref, 1, 6, 0, sRef, 1, 0, Uref, 1, 6, 0, VTref, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );

	info = zgesvd( 'none', 'economy', 6, 3, A, 1, 6, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), Array.from( sRef ), 1e-12, 's' );
});

test( 'zgesvd: 4x3 JOBU=N JOBVT=N (M >= N, values only, path 10)', function t() {
	var tc = findCase( 'economy_4x3' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 1000 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 1 );
	var VT = new Complex128Array( 1 );
	var A = c128( [
		1.0, 0.0, 0.0, 1.0, 2.0, 1.0, 1.0, 2.0,
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0,
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0
	] );
	var info;

	info = zgesvd( 'none', 'none', 4, 3, A, 1, 4, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 1, 0, WORK, 1, 0, 1000, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 4x3 JOBU=O JOBVT=S (M >= N, overwrite A with U)', function t() {
	var tc = findCase( 'economy_4x3' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 1 );   // not used when JOBU='O'
	var VT = new Complex128Array( 9 );  // 3x3
	var A = c128( [
		1.0, 0.0, 0.0, 1.0, 2.0, 1.0, 1.0, 2.0,
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0,
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0
	] );
	var info;

	info = zgesvd( 'overwrite', 'economy', 4, 3, A, 1, 4, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 4x3 JOBU=S JOBVT=O (M >= N, overwrite A with VT)', function t() {
	var tc = findCase( 'economy_4x3' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 12 );   // 4x3
	var VT = new Complex128Array( 1 );   // not used when JOBVT='O'
	var A = c128( [
		1.0, 0.0, 0.0, 1.0, 2.0, 1.0, 1.0, 2.0,
		3.0, 1.0, 1.0, 0.0, 0.0, 2.0, 2.0, 0.0,
		0.0, 3.0, 2.0, 1.0, 1.0, 0.0, 0.0, 1.0
	] );
	var info;

	info = zgesvd( 'economy', 'overwrite', 4, 3, A, 1, 4, 0, s, 1, 0, U, 1, 4, 0, VT, 1, 1, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x5 JOBU=O JOBVT=S (M < N, overwrite A with U)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 1 );   // not used when JOBU='O'
	var VT = new Complex128Array( 15 );  // 3x5
	var A = c128( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var info;

	info = zgesvd( 'overwrite', 'economy', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 1, 0, VT, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x5 JOBU=S JOBVT=O (M < N, overwrite A with VT)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 9 );   // 3x3
	var VT = new Complex128Array( 1 );  // not used when JOBVT='O'
	var A = c128( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var info;

	info = zgesvd( 'economy', 'overwrite', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 3, 0, VT, 1, 1, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x5 JOBU=S JOBVT=S (M < N, economy SVD)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 9 );   // 3x3
	var VT = new Complex128Array( 15 );  // 3x5
	var A = c128( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var info;

	info = zgesvd( 'economy', 'economy', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 3, 0, VT, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 3x5 JOBU=A JOBVT=S (M < N, full U, economy VT)', function t() {
	var tc = findCase( 'full_3x5' );
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 9 );   // 3x3
	var VT = new Complex128Array( 15 );  // 3x5
	var A = c128( [
		2.0, 1.0, 0.0, 3.0, 1.0, 0.0,
		1.0, 1.0, 4.0, 0.0, 0.0, 2.0,
		3.0, 0.0, 1.0, 1.0, 2.0, 2.0,
		0.0, 1.0, 2.0, 0.0, 1.0, 3.0,
		1.0, 2.0, 0.0, 1.0, 3.0, 1.0
	] );
	var info;

	info = zgesvd( 'all-columns', 'economy', 3, 5, A, 1, 3, 0, s, 1, 0, U, 1, 3, 0, VT, 1, 3, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: very small matrix triggers scaling path', function t() {
	// Scale matrix entries to be very small (near underflow) to trigger the anrm < smlnum path
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 2 );
	var sRef = new Float64Array( 2 );
	var U = new Complex128Array( 4 );
	var VT = new Complex128Array( 4 );
	var scale = 1e-160;
	var A1 = c128( [ 3.0*scale, 1.0*scale, 1.0*scale, 2.0*scale, 2.0*scale, 0.0, 4.0*scale, 1.0*scale ] );
	var A2 = c128( [ 3.0, 1.0, 1.0, 2.0, 2.0, 0.0, 4.0, 1.0 ] );
	var info;

	// Get reference singular values at normal scale
	info = zgesvd( 'all-columns', 'all-rows', 2, 2, A2, 1, 2, 0, sRef, 1, 0, U, 1, 2, 0, VT, 1, 2, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'ref info' );

	// Now test at very small scale
	info = zgesvd( 'all-columns', 'all-rows', 2, 2, A1, 1, 2, 0, s, 1, 0, U, 1, 2, 0, VT, 1, 2, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	// Singular values should match the reference scaled by the same factor
	assertSingularValuesClose( [ s[0] / scale, s[1] / scale ], Array.from( sRef ), 1e-8, 'scaled s' );
});

test( 'zgesvd: very large matrix triggers scaling path', function t() {
	// Scale matrix entries to be very large (near overflow) to trigger the anrm > bignum path
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 2 );
	var sRef = new Float64Array( 2 );
	var U = new Complex128Array( 4 );
	var VT = new Complex128Array( 4 );
	var scale = 1e160;
	var A1 = c128( [ 3.0*scale, 1.0*scale, 1.0*scale, 2.0*scale, 2.0*scale, 0.0, 4.0*scale, 1.0*scale ] );
	var A2 = c128( [ 3.0, 1.0, 1.0, 2.0, 2.0, 0.0, 4.0, 1.0 ] );
	var info;

	// Get reference singular values at normal scale
	info = zgesvd( 'all-columns', 'all-rows', 2, 2, A2, 1, 2, 0, sRef, 1, 0, U, 1, 2, 0, VT, 1, 2, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'ref info' );

	// Now test at very large scale
	info = zgesvd( 'all-columns', 'all-rows', 2, 2, A1, 1, 2, 0, s, 1, 0, U, 1, 2, 0, VT, 1, 2, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	assertSingularValuesClose( [ s[0] / scale, s[1] / scale ], Array.from( sRef ), 1e-8, 'scaled s' );
});

test( 'zgesvd: insufficient lwork triggers internal allocation', function t() {
	var tc = findCase( 'full_2x2' );
	var RWORK = new Float64Array( 50 );
	var WORK = new Complex128Array( 1 );  // intentionally small
	var s = new Float64Array( 2 );
	var U = new Complex128Array( 4 );
	var VT = new Complex128Array( 4 );
	var A = c128( [ 3.0, 1.0, 1.0, 2.0, 2.0, 0.0, 4.0, 1.0 ] );
	var info;

	info = zgesvd( 'all-columns', 'all-rows', 2, 2, A, 1, 2, 0, s, 1, 0, U, 1, 2, 0, VT, 1, 2, 0, WORK, 1, 0, 1, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertSingularValuesClose( Array.from( s ), tc.s, 1e-12, 's' );
});

test( 'zgesvd: 5x3 JOBU=S JOBVT=S reconstruction', function t() {
	var RWORK = new Float64Array( 100 );
	var WORK = new Complex128Array( 2500 );
	var s = new Float64Array( 3 );
	var U = new Complex128Array( 15 );   // 5x3 complex
	var VT = new Complex128Array( 9 );  // 3x3 complex
	var M = 5;
	var N = 3;
	var A_orig_f64 = new Float64Array( [
		1.0, 1.0, 2.0, 0.0, 0.0, 1.0, 3.0, 2.0, 1.0, 1.0,
		0.0, 2.0, 1.0, 0.0, 3.0, 1.0, 2.0, 0.0, 0.0, 3.0,
		2.0, 1.0, 0.0, 1.0, 1.0, 2.0, 1.0, 0.0, 2.0, 2.0
	] );
	var A = c128( Array.from( A_orig_f64 ) );
	var Uv;
	var VTv;
	var info;
	var re;
	var im;
	var maxErr;
	var uR;
	var uI;
	var vtR;
	var vtI;
	var sk;
	var errR;
	var errI;
	var i;
	var j;
	var k;
	var minmn = Math.min( M, N );

	info = zgesvd( 'economy', 'economy', M, N, A, 1, M, 0, s, 1, 0, U, 1, M, 0, VT, 1, minmn, 0, WORK, 1, 0, 2500, RWORK, 1, 0 );
	assert.equal( info, 0, 'info' );

	Uv = reinterpret( U, 0 );
	VTv = reinterpret( VT, 0 );

	maxErr = 0.0;
	for ( i = 0; i < M; i++ ) {
		for ( j = 0; j < N; j++ ) {
			re = 0.0;
			im = 0.0;
			for ( k = 0; k < minmn; k++ ) {
				uR = Uv[ 2 * ( i + k * M ) ];
				uI = Uv[ 2 * ( i + k * M ) + 1 ];
				vtR = VTv[ 2 * ( k + j * minmn ) ];
				vtI = VTv[ 2 * ( k + j * minmn ) + 1 ];
				sk = s[ k ];
				re += sk * ( uR * vtR - uI * vtI );
				im += sk * ( uR * vtI + uI * vtR );
			}
			errR = Math.abs( re - A_orig_f64[ 2 * ( i + j * M ) ] );
			errI = Math.abs( im - A_orig_f64[ 2 * ( i + j * M ) + 1 ] );
			maxErr = Math.max( maxErr, errR, errI );
		}
	}
	assert.ok( maxErr < 1e-10, 'reconstruction error: ' + maxErr );
});
