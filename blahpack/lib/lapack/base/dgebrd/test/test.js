'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var Float64Array = require( '@stdlib/array/float64' );
var path = require( 'path' );
var dgebrd = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dgebrd.jsonl' ), 'utf8' ).trim().split( '\n' );
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
* Builds a column-major Float64Array from a flat column-major array.
*/
function colMajor( arr ) {
	return new Float64Array( arr );
}


// TESTS //

test( 'dgebrd: 4x3 upper bidiagonal (M > N)', function t() {
	var tc = findCase( 'upper_4x3' );
	var M = 4;
	var N = 3;
	var A = colMajor([
		2.0, 1.0, 3.0, 1.0,
		1.0, 4.0, 2.0, 3.0,
		3.0, 2.0, 5.0, 1.0
	]);
	var d = new Float64Array( 3 );
	var e = new Float64Array( 2 );
	var TAUQ = new Float64Array( 3 );
	var TAUP = new Float64Array( 3 );
	var WORK = new Float64Array( 100 );
	var info;

	info = dgebrd( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 100 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-14, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
});

test( 'dgebrd: 3x4 lower bidiagonal (M < N)', function t() {
	var tc = findCase( 'lower_3x4' );
	var M = 3;
	var N = 4;
	var A = colMajor([
		2.0, 4.0, 1.0,
		1.0, 2.0, 5.0,
		3.0, 1.0, 2.0,
		1.0, 3.0, 4.0
	]);
	var d = new Float64Array( 3 );
	var e = new Float64Array( 2 );
	var TAUQ = new Float64Array( 3 );
	var TAUP = new Float64Array( 3 );
	var WORK = new Float64Array( 100 );
	var info;

	info = dgebrd( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 100 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-14, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
});

test( 'dgebrd: 3x3 square', function t() {
	var tc = findCase( 'square_3x3' );
	var M = 3;
	var N = 3;
	var A = colMajor([
		5.0, 3.0, 1.0,
		2.0, 4.0, 3.0,
		1.0, 2.0, 6.0
	]);
	var d = new Float64Array( 3 );
	var e = new Float64Array( 2 );
	var TAUQ = new Float64Array( 3 );
	var TAUP = new Float64Array( 3 );
	var WORK = new Float64Array( 100 );
	var info;

	info = dgebrd( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 100 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-14, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
});

test( 'dgebrd: 1x1 matrix', function t() {
	var tc = findCase( 'one_by_one' );
	var A = colMajor([ 7.0 ]);
	var d = new Float64Array( 1 );
	var e = new Float64Array( 0 );
	var TAUQ = new Float64Array( 1 );
	var TAUP = new Float64Array( 1 );
	var WORK = new Float64Array( 10 );
	var info;

	info = dgebrd( 1, 1, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 10 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( A ), tc.A, 1e-14, 'A' );
	assertArrayClose( Array.from( d ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
});

test( 'dgebrd: M=0 quick return', function t() {
	var A = new Float64Array( 1 );
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var TAUQ = new Float64Array( 1 );
	var TAUP = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dgebrd( 0, 3, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 1 );

	assert.equal( info, 0, 'INFO' );
});

test( 'dgebrd: N=0 quick return', function t() {
	var A = new Float64Array( 1 );
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var TAUQ = new Float64Array( 1 );
	var TAUP = new Float64Array( 1 );
	var WORK = new Float64Array( 1 );
	var info;

	info = dgebrd( 3, 0, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 1 );

	assert.equal( info, 0, 'INFO' );
});

test( 'dgebrd: 35x33 upper bidiagonal (blocked path, M > N)', function t() {
	var tc = findCase( 'upper_35x33' );
	var M = 35;
	var N = 33;
	var minmn = Math.min( M, N );
	var A = new Float64Array( M * N );
	var d = new Float64Array( minmn );
	var e = new Float64Array( minmn - 1 );
	var TAUQ = new Float64Array( minmn );
	var TAUP = new Float64Array( minmn );
	var WORK = new Float64Array( ( M + N ) * 32 + 100 );
	var info;
	var i;
	var j;

	// Build the same diagonally dominant matrix as in the Fortran test
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			if ( i === j ) {
				A[ j * M + i ] = ( i + 1 ) + ( j + 1 ) + 10 + Math.sin( ( i + 1 ) + 2 * ( j + 1 ) );
			} else {
				A[ j * M + i ] = Math.sin( ( i + 1 ) + 2 * ( j + 1 ) );
			}
		}
	}

	info = dgebrd( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, WORK.length );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( d ), tc.D, 1e-10, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-10, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-10, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-10, 'TAUP' );
});

test( 'dgebrd: 33x35 lower bidiagonal (blocked path, M < N)', function t() {
	var tc = findCase( 'lower_33x35' );
	var M = 33;
	var N = 35;
	var minmn = Math.min( M, N );
	var A = new Float64Array( M * N );
	var d = new Float64Array( minmn );
	var e = new Float64Array( minmn - 1 );
	var TAUQ = new Float64Array( minmn );
	var TAUP = new Float64Array( minmn );
	var WORK = new Float64Array( ( M + N ) * 32 + 100 );
	var info;
	var i;
	var j;

	// Build the same diagonally dominant matrix as in the Fortran test
	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			if ( i === j ) {
				A[ j * M + i ] = ( i + 1 ) + ( j + 1 ) + 10 + Math.sin( ( i + 1 ) + 2 * ( j + 1 ) );
			} else {
				A[ j * M + i ] = Math.sin( ( i + 1 ) + 2 * ( j + 1 ) );
			}
		}
	}

	info = dgebrd( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, WORK.length );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( d ), tc.D, 1e-10, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-10, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-10, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-10, 'TAUP' );
});

test( 'dgebrd: 35x33 with limited workspace (falls back to unblocked)', function t() {
	// This test exercises the branch where lwork < (M+N)*nbmin, causing nb=1
	var tc = findCase( 'upper_35x33' );
	var M = 35;
	var N = 33;
	var minmn = Math.min( M, N );
	var A = new Float64Array( M * N );
	var d = new Float64Array( minmn );
	var e = new Float64Array( minmn - 1 );
	var TAUQ = new Float64Array( minmn );
	var TAUP = new Float64Array( minmn );
	// lwork = 100 < (35+33)*2 = 136, so nb falls to 1 (unblocked)
	var WORK = new Float64Array( 100 );
	var info;
	var i;
	var j;

	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			if ( i === j ) {
				A[ j * M + i ] = ( i + 1 ) + ( j + 1 ) + 10 + Math.sin( ( i + 1 ) + 2 * ( j + 1 ) );
			} else {
				A[ j * M + i ] = Math.sin( ( i + 1 ) + 2 * ( j + 1 ) );
			}
		}
	}

	info = dgebrd( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, 100 );

	assert.equal( info, tc.INFO, 'INFO' );
	// Same result as fully blocked since the algorithm is mathematically equivalent
	assertArrayClose( Array.from( d ), tc.D, 1e-10, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-10, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-10, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-10, 'TAUP' );
});

test( 'dgebrd: 35x33 with partially limited workspace (reduced NB)', function t() {
	// This test exercises the branch where lwork >= (M+N)*nbmin but < (M+N)*nb
	// so nb is reduced but still > 1
	var tc = findCase( 'upper_35x33' );
	var M = 35;
	var N = 33;
	var minmn = Math.min( M, N );
	var A = new Float64Array( M * N );
	var d = new Float64Array( minmn );
	var e = new Float64Array( minmn - 1 );
	var TAUQ = new Float64Array( minmn );
	var TAUP = new Float64Array( minmn );
	// lwork = (35+33)*4 = 272, enough for nb=4 but < (35+33)*32 = 2176
	var lwork = ( M + N ) * 4;
	var WORK = new Float64Array( lwork );
	var info;
	var i;
	var j;

	for ( j = 0; j < N; j++ ) {
		for ( i = 0; i < M; i++ ) {
			if ( i === j ) {
				A[ j * M + i ] = ( i + 1 ) + ( j + 1 ) + 10 + Math.sin( ( i + 1 ) + 2 * ( j + 1 ) );
			} else {
				A[ j * M + i ] = Math.sin( ( i + 1 ) + 2 * ( j + 1 ) );
			}
		}
	}

	info = dgebrd( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0, lwork );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( d ), tc.D, 1e-10, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-10, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-10, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-10, 'TAUP' );
});

test( 'dgebrd: null WORK causes internal allocation', function t() {
	var tc = findCase( 'square_3x3' );
	var M = 3;
	var N = 3;
	var A = colMajor([
		5.0, 3.0, 1.0,
		2.0, 4.0, 3.0,
		1.0, 2.0, 6.0
	]);
	var d = new Float64Array( 3 );
	var e = new Float64Array( 2 );
	var TAUQ = new Float64Array( 3 );
	var TAUP = new Float64Array( 3 );
	var info;

	info = dgebrd( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, null, 1, 0, 0 );

	assert.equal( info, tc.INFO, 'INFO' );
	assertArrayClose( Array.from( d ), tc.D, 1e-14, 'D' );
	assertArrayClose( Array.from( e ), tc.E, 1e-14, 'E' );
	assertArrayClose( Array.from( TAUQ ), tc.TAUQ, 1e-14, 'TAUQ' );
	assertArrayClose( Array.from( TAUP ), tc.TAUP, 1e-14, 'TAUP' );
});
