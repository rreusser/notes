'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlascl = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlascl.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr;
	if ( expected === 0.0 ) {
		assert.ok( Math.abs( actual ) <= tol, msg + ': expected ' + expected + ', got ' + actual );
		return;
	}
	relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
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

test( 'dlascl: general_basic - scale 3x2 general matrix by 2', function t() {
	var tc = findCase( 'general_basic' );
	// A = [1 4; 2 5; 3 6] column-major, M=3, N=2, LDA=3
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var info = dlascl( 'G', 0, 0, 1.0, 2.0, 3, 2, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: general_half - scale by 0.5 (cfrom=2, cto=1)', function t() {
	var tc = findCase( 'general_half' );
	// A = [10 30; 20 40] column-major, M=2, N=2, LDA=2
	var A = new Float64Array( [ 10, 20, 30, 40 ] );
	var info = dlascl( 'G', 0, 0, 2.0, 1.0, 2, 2, A, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: m_zero - M=0 quick return', function t() {
	var tc = findCase( 'm_zero' );
	var A = new Float64Array( [ 99.0 ] );
	var info = dlascl( 'G', 0, 0, 1.0, 2.0, 0, 2, A, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: n_zero - N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( [ 99.0 ] );
	var info = dlascl( 'G', 0, 0, 1.0, 2.0, 2, 0, A, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: lower_tri - lower triangular 3x3', function t() {
	var tc = findCase( 'lower_tri' );
	// A column-major LDA=3:
	// col0: [1, 2, 3], col1: [0, 4, 5], col2: [0, 0, 6]
	var A = new Float64Array( [ 1, 2, 3, 0, 4, 5, 0, 0, 6 ] );
	var info = dlascl( 'L', 0, 0, 1.0, 3.0, 3, 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: upper_tri - upper triangular 3x3', function t() {
	var tc = findCase( 'upper_tri' );
	// A column-major LDA=3:
	// col0: [1, 0, 0], col1: [2, 4, 0], col2: [3, 5, 6]
	var A = new Float64Array( [ 1, 0, 0, 2, 4, 0, 3, 5, 6 ] );
	var info = dlascl( 'U', 0, 0, 1.0, 3.0, 3, 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: hessenberg - upper Hessenberg 3x3', function t() {
	var tc = findCase( 'hessenberg' );
	// A column-major LDA=3:
	// col0: [1, 2, 0], col1: [3, 4, 5], col2: [6, 7, 8]
	var A = new Float64Array( [ 1, 2, 0, 3, 4, 5, 6, 7, 8 ] );
	var info = dlascl( 'H', 0, 0, 1.0, 2.0, 3, 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: identity - cfrom=cto, MUL=1 quick return', function t() {
	var tc = findCase( 'identity' );
	var A = new Float64Array( [ 1, 2, 3, 4 ] );
	var info = dlascl( 'G', 0, 0, 5.0, 5.0, 2, 2, A, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: large_ratio - cfrom=1e300, cto=1e-300 (iterative scaling down)', function t() {
	var tc = findCase( 'large_ratio' );
	var A = new Float64Array( [ 1.0, 2.0 ] );
	var info = dlascl( 'G', 0, 0, 1e300, 1e-300, 2, 1, A, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: large_ratio_inv - cfrom=1e-150, cto=1e150 (iterative scaling up)', function t() {
	var tc = findCase( 'large_ratio_inv' );
	var A = new Float64Array( [ 1e-150, 2e-150 ] );
	var info = dlascl( 'G', 0, 0, 1e-150, 1e150, 2, 1, A, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: lower_band - type B, kl=ku=1, 4x4', function t() {
	var tc = findCase( 'lower_band' );
	// Band storage: kl+1=2 rows by N=4 columns, LDA=2
	var A = new Float64Array( [
		1, 4,   // col 0: diag, sub
		2, 5,   // col 1: diag, sub
		3, 6,   // col 2: diag, sub
		7, 0    // col 3: diag only (k4-j limits)
	] );
	var info = dlascl( 'B', 1, 1, 1.0, 3.0, 4, 4, A, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: upper_band - type Q, kl=ku=1, 4x4', function t() {
	var tc = findCase( 'upper_band' );
	// Band storage: ku+1=2 rows by N=4 columns, LDA=2
	var A = new Float64Array( [
		0, 4,   // col 0: unused super, diag
		1, 5,   // col 1: super, diag
		2, 6,   // col 2: super, diag
		3, 7    // col 3: super, diag
	] );
	var info = dlascl( 'Q', 1, 1, 1.0, 3.0, 4, 4, A, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: full_band - type Z, kl=1, ku=1, 3x3', function t() {
	var tc = findCase( 'full_band' );
	// Band storage: 2*kl+ku+1=4 rows by N=3 columns, LDA=4
	var A = new Float64Array( [
		0, 3, 6, 9,     // col 0
		1, 4, 7, 10,    // col 1
		2, 5, 8, 0      // col 2
	] );
	var info = dlascl( 'Z', 1, 1, 1.0, 2.0, 3, 3, A, 1, 4, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: general_rect - non-square 2x4 general matrix', function t() {
	var tc = findCase( 'general_rect' );
	// A = [1 3 5 7; 2 4 6 8] column-major, M=2, N=4, LDA=2
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6, 7, 8 ] );
	var info = dlascl( 'G', 0, 0, 1.0, 10.0, 2, 4, A, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: lower_rect - lower triangular 4x3', function t() {
	var tc = findCase( 'lower_rect' );
	// 4x3 lower triangular, LDA=4, column-major
	var A = new Float64Array( [
		1, 2, 3, 4,   // col 0
		0, 5, 6, 7,   // col 1
		0, 0, 8, 9    // col 2
	] );
	var info = dlascl( 'L', 0, 0, 1.0, 2.0, 4, 3, A, 1, 4, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: upper_rect - upper triangular 3x4', function t() {
	var tc = findCase( 'upper_rect' );
	// 3x4 upper triangular, LDA=3, column-major
	var A = new Float64Array( [
		1, 0, 0,   // col 0
		2, 3, 0,   // col 1
		4, 5, 6,   // col 2
		7, 8, 9    // col 3
	] );
	var info = dlascl( 'U', 0, 0, 1.0, 2.0, 3, 4, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dlascl: invalid type returns -1', function t() {
	var A = new Float64Array( [ 1, 2, 3, 4 ] );
	var info = dlascl( 'X', 0, 0, 1.0, 2.0, 2, 2, A, 1, 2, 0 );
	assert.equal( info, -1 );
});

test( 'dlascl: offset support - general matrix with offset', function t() {
	// Place a 2x2 matrix starting at offset 3
	var A = new Float64Array( [ 0, 0, 0, 1, 2, 3, 4 ] );
	var info = dlascl( 'G', 0, 0, 1.0, 5.0, 2, 2, A, 1, 2, 3 );
	assert.equal( info, 0 );
	// First 3 elements should be unchanged
	assert.equal( A[ 0 ], 0 );
	assert.equal( A[ 1 ], 0 );
	assert.equal( A[ 2 ], 0 );
	// Elements at offset 3..6 should be scaled by 5
	assert.equal( A[ 3 ], 5 );
	assert.equal( A[ 4 ], 10 );
	assert.equal( A[ 5 ], 15 );
	assert.equal( A[ 6 ], 20 );
});

test( 'dlascl: case-insensitive type', function t() {
	var A = new Float64Array( [ 1, 2, 3, 4 ] );
	var info = dlascl( 'g', 0, 0, 1.0, 2.0, 2, 2, A, 1, 2, 0 );
	assert.equal( info, 0 );
	assert.equal( A[ 0 ], 2 );
	assert.equal( A[ 1 ], 4 );
	assert.equal( A[ 2 ], 6 );
	assert.equal( A[ 3 ], 8 );
});

test( 'dlascl: cfrom=Infinity triggers single-pass scaling', function t() {
	// When cfrom is Infinity, cfrom1=cfromc*smlnum=Inf, so cfrom1===cfromc
	// mul = cto/cfromc = finite/Inf = 0.0
	var A = new Float64Array( [ 5, 10 ] );
	var info = dlascl( 'G', 0, 0, Infinity, 1.0, 2, 1, A, 1, 2, 0 );
	assert.equal( info, 0 );
	assert.equal( A[ 0 ], 0 );
	assert.equal( A[ 1 ], 0 );
});

test( 'dlascl: very large upward ratio triggers mul=bignum branch', function t() {
	// cfrom=1e-300, cto=1e300 => ratio 1e600, needs multiple iterations via bignum
	// Result: 1e-300 * (1e300/1e-300) = 1e-300 * 1e600 = 1e300
	var A = new Float64Array( [ 1e-300 ] );
	var info = dlascl( 'G', 0, 0, 1e-300, 1e300, 1, 1, A, 1, 1, 0 );
	assert.equal( info, 0 );
	assertClose( A[ 0 ], 1e300, 1e-14, 'scaled value' );
});

test( 'dlascl: cto=0 zeroes the matrix', function t() {
	// When cto=0, cto1 = ctoc/bignum = 0, so cto1===ctoc
	// mul = ctoc = 0, cfromc = 1
	var A = new Float64Array( [ 5, 10, 15, 20 ] );
	var info = dlascl( 'G', 0, 0, 1.0, 0.0, 2, 2, A, 1, 2, 0 );
	assert.equal( info, 0 );
	assert.equal( A[ 0 ], 0 );
	assert.equal( A[ 1 ], 0 );
	assert.equal( A[ 2 ], 0 );
	assert.equal( A[ 3 ], 0 );
});
