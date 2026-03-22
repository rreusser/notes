

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var zgebd2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgebd2.jsonl' ), 'utf8' ).trim().split( '\n' );
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


// TESTS //

test( 'zgebd2: upper_4x3 (M > N, upper bidiagonal)', function t() {
	var tc = findCase( 'upper_4x3' );
	var M = 4;
	var N = 3;
	var A = new Float64Array([
		1, 2,  3, 4,  5, 6,  7, 8,
		9, 1,  2, 3,  4, 5,  6, 7,
		8, 9,  1, 2,  3, 4,  5, 6
	]);
	var d = new Float64Array( 3 );
	var e = new Float64Array( 2 );
	var TAUQ = new Float64Array( 6 );
	var TAUP = new Float64Array( 6 );
	var WORK = new Float64Array( 2 * Math.max( M, N ) );
	var info = zgebd2( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( Array.from( e ), tc.e, 1e-14, 'e' );
	assertArrayClose( Array.from( TAUQ ), tc.tauq, 1e-14, 'tauq' );
	assertArrayClose( Array.from( TAUP ), tc.taup, 1e-14, 'taup' );
});

test( 'zgebd2: square_3x3 (M >= N, upper bidiagonal)', function t() {
	var tc = findCase( 'square_3x3' );
	var M = 3;
	var N = 3;
	// Column-major: col 0 = [a(1,1), a(2,1), a(3,1)], col 1 = [a(1,2), a(2,2), a(3,2)], ...
	var A = new Float64Array([
		1, 1,  2, -1,  0, 3,
		4, 0,  5, 2,   1, -1,
		3, 1,  0, 4,   2, 2
	]);
	var d = new Float64Array( 3 );
	var e = new Float64Array( 2 );
	var TAUQ = new Float64Array( 6 );
	var TAUP = new Float64Array( 6 );
	var WORK = new Float64Array( 2 * Math.max( M, N ) );
	var info = zgebd2( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( Array.from( e ), tc.e, 1e-14, 'e' );
	assertArrayClose( Array.from( TAUQ ), tc.tauq, 1e-14, 'tauq' );
	assertArrayClose( Array.from( TAUP ), tc.taup, 1e-14, 'taup' );
});

test( 'zgebd2: lower_3x4 (M < N, lower bidiagonal)', function t() {
	var tc = findCase( 'lower_3x4' );
	var M = 3;
	var N = 4;
	// Column-major with LDA=3: col 0 has 3 elements, etc.
	var A = new Float64Array([
		1, 2,  3, 4,  5, 6,
		7, 8,  9, 1,  2, 3,
		4, 5,  6, 7,  8, 9,
		1, 2,  3, 4,  5, 6
	]);
	var d = new Float64Array( 3 );
	var e = new Float64Array( 2 );
	var TAUQ = new Float64Array( 6 );
	var TAUP = new Float64Array( 6 );
	var WORK = new Float64Array( 2 * Math.max( M, N ) );
	var info = zgebd2( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( Array.from( e ), tc.e, 1e-14, 'e' );
	assertArrayClose( Array.from( TAUQ ), tc.tauq, 1e-14, 'tauq' );
	assertArrayClose( Array.from( TAUP ), tc.taup, 1e-14, 'taup' );
});

test( 'zgebd2: m_zero (quick return)', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Float64Array( 2 );
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var TAUQ = new Float64Array( 2 );
	var TAUP = new Float64Array( 2 );
	var WORK = new Float64Array( 6 );
	var info = zgebd2( 0, 3, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zgebd2: n_zero (quick return)', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 2 );
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var TAUQ = new Float64Array( 2 );
	var TAUP = new Float64Array( 2 );
	var WORK = new Float64Array( 6 );
	var info = zgebd2( 3, 0, A, 1, 3, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zgebd2: one_by_one', function t() {
	var tc = findCase( 'one_by_one' );
	var A = new Float64Array([ 5, 3 ]);
	var d = new Float64Array( 1 );
	var e = new Float64Array( 1 );
	var TAUQ = new Float64Array( 2 );
	var TAUP = new Float64Array( 2 );
	var WORK = new Float64Array( 2 );
	var info = zgebd2( 1, 1, A, 1, 1, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( Array.from( TAUQ ), tc.tauq, 1e-14, 'tauq' );
	assertArrayClose( Array.from( TAUP ), tc.taup, 1e-14, 'taup' );
});

test( 'zgebd2: lower_2x3 (M < N, lower bidiagonal, small)', function t() {
	var tc = findCase( 'lower_2x3' );
	var M = 2;
	var N = 3;
	// Column-major with LDA=2
	var A = new Float64Array([
		1, 0,  0, 1,
		2, 1,  1, -1,
		3, 0,  0, 2
	]);
	var d = new Float64Array( 2 );
	var e = new Float64Array( 1 );
	var TAUQ = new Float64Array( 4 );
	var TAUP = new Float64Array( 4 );
	var WORK = new Float64Array( 2 * Math.max( M, N ) );
	var info = zgebd2( M, N, A, 1, M, 0, d, 1, 0, e, 1, 0, TAUQ, 1, 0, TAUP, 1, 0, WORK, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
	assertArrayClose( Array.from( d ), tc.d, 1e-14, 'd' );
	assertArrayClose( Array.from( e ), tc.e, 1e-14, 'e' );
	assertArrayClose( Array.from( TAUQ ), tc.tauq, 1e-14, 'tauq' );
	assertArrayClose( Array.from( TAUP ), tc.taup, 1e-14, 'taup' );
});
