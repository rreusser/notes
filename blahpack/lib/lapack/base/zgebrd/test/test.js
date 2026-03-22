

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zgebrd = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgebrd.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Generates a diagonally dominant complex matrix.
* A(i,j) = (delta_ij * (i+j+10) + sin(i+2*j), cos(2*i+j))
* Matches the Fortran test (1-based i,j in Fortran = 0-based i,j + 1 here).
*
* @param {number} M - rows
* @param {number} N - columns
* @returns {Float64Array} interleaved complex array, column-major
*/
function makeBigMatrix( M, N ) {
	var A = new Float64Array( 2 * M * N );
	var i;
	var j;
	var idx;
	var fi;
	var fj;
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			fi = i + 1; // 1-based
			fj = j + 1;
			idx = 2 * ( j * M + i );
			if ( i === j ) {
				A[ idx ] = ( fi + fj + 10 ) + Math.sin( fi + 2 * fj );
				A[ idx + 1 ] = Math.cos( 2 * fi + fj );
			} else {
				A[ idx ] = Math.sin( fi + 2 * fj );
				A[ idx + 1 ] = Math.cos( 2 * fi + fj );
			}
		}
	}
	return A;
}


// TESTS //

test( 'zgebrd: upper_4x3 (M > N, upper bidiagonal)', function t() {
	var tc = findCase( 'upper_4x3' );
	var info;

	// 4x3 matrix, column-major, LDA=4
	// a(1)=(1,2) a(2)=(3,4) a(3)=(5,6) a(4)=(7,8)   <- col 1
	// a(5)=(9,1) a(6)=(2,3) a(7)=(4,5) a(8)=(6,7)   <- col 2
	// a(9)=(8,9) a(10)=(1,2) a(11)=(3,4) a(12)=(5,6) <- col 3
	var A = new Float64Array([
		1, 2, 3, 4, 5, 6, 7, 8,
		9, 1, 2, 3, 4, 5, 6, 7,
		8, 9, 1, 2, 3, 4, 5, 6
	]);
	var d = new Float64Array( 3 );
	var e = new Float64Array( 2 );
	var TAUQ = new Float64Array( 6 );
	var TAUP = new Float64Array( 6 );
	var WORK = new Float64Array( 200 );

	info = zgebrd( 4, 3, A, 1, 4, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 100 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( Array.from( e ), tc.e, 1e-14, 'e' );
	assertArrayClose( Array.from( TAUQ ), tc.tauq, 1e-14, 'tauq' );
	assertArrayClose( Array.from( TAUP ), tc.taup, 1e-14, 'taup' );
});

test( 'zgebrd: lower_3x4 (M < N, lower bidiagonal)', function t() {
	var tc = findCase( 'lower_3x4' );
	var info;

	// 3x4 matrix, column-major, LDA=3
	var A = new Float64Array([
		1, 2, 3, 4, 5, 6,
		7, 8, 9, 1, 2, 3,
		4, 5, 6, 7, 8, 9,
		1, 2, 3, 4, 5, 6
	]);
	var d = new Float64Array( 3 );
	var e = new Float64Array( 2 );
	var TAUQ = new Float64Array( 6 );
	var TAUP = new Float64Array( 6 );
	var WORK = new Float64Array( 200 );

	info = zgebrd( 3, 4, A, 1, 3, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 100 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( Array.from( e ), tc.e, 1e-14, 'e' );
	assertArrayClose( Array.from( TAUQ ), tc.tauq, 1e-14, 'tauq' );
	assertArrayClose( Array.from( TAUP ), tc.taup, 1e-14, 'taup' );
});

test( 'zgebrd: square_6x6', function t() {
	var tc = findCase( 'square_6x6' );
	var info;

	// 6x6 well-conditioned matrix, column-major, LDA=6
	var A = new Float64Array([
		5, 1, 0, 2, 1, -1, 0, 0, 3, 0, 0, 1,
		2, 0, 6, -1, 0, 3, 1, 0, 0, 0, 4, 2,
		0, 1, 1, 0, 7, 0, 0, -2, 2, 1, 0, 0,
		3, 0, 0, 1, 0, -1, 8, 0, 1, 0, 0, 3,
		1, -1, 0, 0, 2, 0, 0, 1, 9, 0, 1, -2,
		0, 2, 1, -1, 0, 0, 3, 0, 0, 1, 10, 0
	]);
	var d = new Float64Array( 6 );
	var e = new Float64Array( 5 );
	var TAUQ = new Float64Array( 12 );
	var TAUP = new Float64Array( 12 );
	var WORK = new Float64Array( 800 );

	info = zgebrd( 6, 6, A, 1, 6, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 400 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-13, 'a' );
	assertArrayClose( Array.from( d ), tc.d, 1e-13, 'd' );
	assertArrayClose( Array.from( e ), tc.e, 1e-13, 'e' );
	assertArrayClose( Array.from( TAUQ ), tc.tauq, 1e-13, 'tauq' );
	assertArrayClose( Array.from( TAUP ), tc.taup, 1e-13, 'taup' );
});

test( 'zgebrd: m_zero (quick return)', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Float64Array( 1 );
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var TAUQ = new Float64Array( 2 );
	var TAUP = new Float64Array( 2 );
	var WORK = new Float64Array( 10 );
	var info;

	info = zgebrd( 0, 3, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 5 );

	assert.equal( info, tc.info );
});

test( 'zgebrd: n_zero (quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var TAUQ = new Float64Array( 2 );
	var TAUP = new Float64Array( 2 );
	var WORK = new Float64Array( 10 );
	var info;

	info = zgebrd( 3, 0, A, 1, 3, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 5 );

	assert.equal( info, tc.info );
});

test( 'zgebrd: one_by_one', function t() {
	var tc = findCase( 'one_by_one' );
	var info;

	var A = new Float64Array([ 5, 3 ]);
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var TAUQ = new Float64Array( 2 );
	var TAUP = new Float64Array( 2 );
	var WORK = new Float64Array( 10 );

	info = zgebrd( 1, 1, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 5 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( Array.from( TAUQ ), tc.tauq, 1e-14, 'tauq' );
	assertArrayClose( Array.from( TAUP ), tc.taup, 1e-14, 'taup' );
});

test( 'zgebrd: upper_35x33 (blocked, M > N, small lwork)', function t() {
	// Test with lwork smaller than optimal to exercise workspace reduction logic
	var tc = findCase( 'upper_35x33' );
	var WORK;
	var TAUQ;
	var TAUP;
	var info;
	var A;
	var d;
	var e;

	A = makeBigMatrix( 35, 33 );
	d = new Float64Array( 33 );
	e = new Float64Array( 32 );
	TAUQ = new Float64Array( 66 );
	TAUP = new Float64Array( 66 );

	// Provide lwork = (M+N)*2 which is enough for nbmin=2 but not for nb=32
	var smallLwork = ( 35 + 33 ) * 2;
	WORK = new Float64Array( 2 * smallLwork );

	info = zgebrd( 35, 33, A, 1, 35, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, smallLwork );

	assert.equal( info, tc.info );
	// D and E should still match (same mathematical result, different blocking)
	assertArrayClose( Array.from( d ), tc.d, 1e-10, 'd' );
	assertArrayClose( Array.from( e ), tc.e, 1e-10, 'e' );
});

test( 'zgebrd: upper_35x33 (blocked, M > N, tiny lwork)', function t() {
	// Test with lwork < (M+N)*2, which forces nb=1 (unblocked path)
	var tc = findCase( 'upper_35x33' );
	var WORK;
	var TAUQ;
	var TAUP;
	var info;
	var A;
	var d;
	var e;

	A = makeBigMatrix( 35, 33 );
	d = new Float64Array( 33 );
	e = new Float64Array( 32 );
	TAUQ = new Float64Array( 66 );
	TAUP = new Float64Array( 66 );

	// Provide lwork = 35 (minimum for unblocked = max(M,N))
	var tinyLwork = 35;
	WORK = new Float64Array( 2 * tinyLwork );

	info = zgebrd( 35, 33, A, 1, 35, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, tinyLwork );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-10, 'd' );
	assertArrayClose( Array.from( e ), tc.e, 1e-10, 'e' );
});

test( 'zgebrd: upper_35x33 (blocked, M > N)', function t() {
	var tc = findCase( 'upper_35x33' );
	var WORK;
	var TAUQ;
	var TAUP;
	var info;
	var A;
	var d;
	var e;

	A = makeBigMatrix( 35, 33 );
	d = new Float64Array( 33 );
	e = new Float64Array( 32 );
	TAUQ = new Float64Array( 66 );
	TAUP = new Float64Array( 66 );
	WORK = new Float64Array( 2 * ( 35 + 33 ) * 32 );

	info = zgebrd( 35, 33, A, 1, 35, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, ( 35 + 33 ) * 32 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-12, 'd' );
	assertArrayClose( Array.from( e ), tc.e, 1e-12, 'e' );
	assertArrayClose( Array.from( TAUQ ), tc.tauq, 1e-12, 'tauq' );
	assertArrayClose( Array.from( TAUP ), tc.taup, 1e-12, 'taup' );
});

test( 'zgebrd: lower_33x35 (blocked, M < N)', function t() {
	var tc = findCase( 'lower_33x35' );
	var WORK;
	var TAUQ;
	var TAUP;
	var info;
	var A;
	var d;
	var e;

	A = makeBigMatrix( 33, 35 );
	d = new Float64Array( 33 );
	e = new Float64Array( 32 );
	TAUQ = new Float64Array( 66 );
	TAUP = new Float64Array( 66 );
	WORK = new Float64Array( 2 * ( 33 + 35 ) * 32 );

	info = zgebrd( 33, 35, A, 1, 33, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, ( 33 + 35 ) * 32 );

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( d ), tc.d, 1e-12, 'd' );
	assertArrayClose( Array.from( e ), tc.e, 1e-12, 'e' );
	assertArrayClose( Array.from( TAUQ ), tc.tauq, 1e-12, 'tauq' );
	assertArrayClose( Array.from( TAUP ), tc.taup, 1e-12, 'taup' );
});
