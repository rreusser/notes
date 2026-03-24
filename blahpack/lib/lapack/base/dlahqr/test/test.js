

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dlahqr = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlahqr.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// FUNCTIONS //

function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name; } );
}

function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 );
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual + ' (relErr=' + relErr + ')' );
}

function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

function identityMatrix( N ) {
	var Z = new Float64Array( N * N );
	var i;
	for ( i = 0; i < N; i++ ) {
		Z[ i + i * N ] = 1.0;
	}
	return Z;
}



// TESTS //

test( 'dlahqr: real_eigenvalues_4x4', function t() {
	var tc = findCase( 'real_eigenvalues_4x4' );
	var N = 4;
	var H = new Float64Array( [
		4.0, 1.0, 0.0, 0.0,
		3.0, 4.0, 1.0, 0.0,
		2.0, 3.0, 4.0, 1.0,
		1.0, 2.0, 3.0, 4.0
	] );
	var Z = identityMatrix( N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var info = dlahqr( true, true, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( WR ), tc.wr, 1e-13, 'wr' );
	assertArrayClose( Array.from( WI ), tc.wi, 1e-13, 'wi' );
	assertArrayClose( Array.from( H ), tc.h, 1e-13, 'h' );
	assertArrayClose( Array.from( Z ), tc.z, 1e-13, 'z' );
});

test( 'dlahqr: complex_eigenvalues_4x4', function t() {
	var tc = findCase( 'complex_eigenvalues_4x4' );
	var N = 4;
	var H = new Float64Array( [
		0.0,  1.0, 0.0, 0.0,
		-1.0, 0.0, 1.0, 0.0,
		2.0,  1.0, 0.0, 1.0,
		1.0,  2.0, -1.0, 0.0
	] );
	var Z = identityMatrix( N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var info = dlahqr( true, true, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( WR ), tc.wr, 1e-13, 'wr' );
	assertArrayClose( Array.from( WI ), tc.wi, 1e-13, 'wi' );
	assertArrayClose( Array.from( H ), tc.h, 1e-13, 'h' );
	assertArrayClose( Array.from( Z ), tc.z, 1e-13, 'z' );
});

test( 'dlahqr: triangular_3x3', function t() {
	var tc = findCase( 'triangular_3x3' );
	var N = 3;
	var H = new Float64Array( [
		1.0, 0.0, 0.0,
		2.0, 4.0, 0.0,
		3.0, 5.0, 6.0
	] );
	var Z = identityMatrix( N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var info = dlahqr( true, true, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( WR ), tc.wr, 1e-14, 'wr' );
	assertArrayClose( Array.from( WI ), tc.wi, 1e-14, 'wi' );
	// H fixture is column-major from Fortran (LDH=6), but we use LDH=N=3
	// The triangular matrix should remain unchanged since it's already in Schur form
});

test( 'dlahqr: eigenvalues_only_4x4', function t() {
	var tc = findCase( 'eigenvalues_only_4x4' );
	var N = 4;
	var H = new Float64Array( [
		4.0, 1.0, 0.0, 0.0,
		3.0, 4.0, 1.0, 0.0,
		2.0, 3.0, 4.0, 1.0,
		1.0, 2.0, 3.0, 4.0
	] );
	var Z = new Float64Array( N * N ); // unused
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var info = dlahqr( false, false, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( WR ), tc.wr, 1e-13, 'wr' );
	assertArrayClose( Array.from( WI ), tc.wi, 1e-13, 'wi' );
});

test( 'dlahqr: ilo_eq_ihi', function t() {
	var tc = findCase( 'ilo_eq_ihi' );
	var N = 4;
	var H = new Float64Array( [
		5.0, 0.0, 0.0, 0.0,
		3.0, 4.0, 0.0, 0.0,
		2.0, 3.0, 3.0, 0.0,
		1.0, 2.0, 1.0, 7.0
	] );
	var Z = new Float64Array( N * N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var info = dlahqr( true, false, N, 2, 2, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0 );
	assert.equal( info, tc.info );
	assertClose( WR[ 1 ], tc.wr2, 1e-14, 'wr2' );
	assertClose( WI[ 1 ], tc.wi2, 1e-14, 'wi2' );
});

test( 'dlahqr: n0', function t() {
	var tc = findCase( 'n0' );
	var H = new Float64Array( 1 );
	var Z = new Float64Array( 1 );
	var WR = new Float64Array( 1 );
	var WI = new Float64Array( 1 );
	var info = dlahqr( true, true, 0, 1, 0, H, 1, 1, 0, WR, 1, 0, WI, 1, 0, 1, 0, Z, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dlahqr: 2x2_complex', function t() {
	var tc = findCase( '2x2_complex' );
	var N = 2;
	// Column-major: col0=[0,1], col1=[-2,0]
	var H = new Float64Array( [
		0.0,  1.0,
		-2.0, 0.0
	] );
	var Z = identityMatrix( N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var info = dlahqr( true, true, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( WR ), tc.wr, 1e-14, 'wr' );
	assertArrayClose( Array.from( WI ), tc.wi, 1e-14, 'wi' );
	assertArrayClose( Array.from( H ), tc.h, 1e-14, 'h' );
	assertArrayClose( Array.from( Z ), tc.z, 1e-14, 'z' );
});

test( 'dlahqr: partial_range_6x6', function t() {
	var tc = findCase( 'partial_range_6x6' );
	var N = 6;
	// Build column-major 6x6 from the Fortran test inputs
	// Row-by-row in Fortran:
	// H(1,1)=10, H(1,2)=1, H(1,3)=2, H(1,4)=3, H(1,5)=4, H(1,6)=5
	// H(2,2)=4, H(2,3)=3, H(2,4)=1, H(2,5)=0.5, H(2,6)=0.1
	// H(3,2)=1, H(3,3)=3, H(3,4)=2, H(3,5)=1, H(3,6)=0.2
	// H(4,3)=0.5, H(4,4)=2, H(4,5)=1.5, H(4,6)=0.3
	// H(5,4)=0.25, H(5,5)=1, H(5,6)=0.4
	// H(6,6)=20
	var H = new Float64Array( N * N );
	H[ 0 + 0 * N ] = 10.0; H[ 0 + 1 * N ] = 1.0;  H[ 0 + 2 * N ] = 2.0;  H[ 0 + 3 * N ] = 3.0;  H[ 0 + 4 * N ] = 4.0;  H[ 0 + 5 * N ] = 5.0;
	H[ 1 + 1 * N ] = 4.0;  H[ 1 + 2 * N ] = 3.0;  H[ 1 + 3 * N ] = 1.0;  H[ 1 + 4 * N ] = 0.5;  H[ 1 + 5 * N ] = 0.1;
	H[ 2 + 1 * N ] = 1.0;  H[ 2 + 2 * N ] = 3.0;  H[ 2 + 3 * N ] = 2.0;  H[ 2 + 4 * N ] = 1.0;  H[ 2 + 5 * N ] = 0.2;
	H[ 3 + 2 * N ] = 0.5;  H[ 3 + 3 * N ] = 2.0;  H[ 3 + 4 * N ] = 1.5;  H[ 3 + 5 * N ] = 0.3;
	H[ 4 + 3 * N ] = 0.25; H[ 4 + 4 * N ] = 1.0;  H[ 4 + 5 * N ] = 0.4;
	H[ 5 + 5 * N ] = 20.0;

	var Z = identityMatrix( N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var info = dlahqr( true, true, N, 2, 5, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0 );
	assert.equal( info, tc.info );

	// Fixture H is column-major with LDH=6 (same as N here)
	assertArrayClose( Array.from( WR ).slice( 1, 5 ), tc.wr.slice( 1, 5 ), 1e-12, 'wr' );
	assertArrayClose( Array.from( WI ).slice( 1, 5 ), tc.wi.slice( 1, 5 ), 1e-12, 'wi' );
	assertArrayClose( Array.from( H ), tc.h, 1e-12, 'h' );
	assertArrayClose( Array.from( Z ), tc.z, 1e-12, 'z' );
});

test( 'dlahqr: mixed_eigenvalues_5x5', function t() {
	var tc = findCase( 'mixed_eigenvalues_5x5' );
	var N = 5;
	var H = new Float64Array( N * N );
	// Column-major from Fortran row inputs:
	// row 0: [5, 4, 1, 0.5, 0.1]
	// row 1: [1, 3, 2, 1, 0.5]
	// row 2: [0, 2, 1, 3, 1]
	// row 3: [0, 0, 1.5, 2, 2]
	// row 4: [0, 0, 0, 0.5, 4]
	H[ 0 + 0 * N ] = 5.0; H[ 0 + 1 * N ] = 4.0; H[ 0 + 2 * N ] = 1.0;  H[ 0 + 3 * N ] = 0.5; H[ 0 + 4 * N ] = 0.1;
	H[ 1 + 0 * N ] = 1.0; H[ 1 + 1 * N ] = 3.0; H[ 1 + 2 * N ] = 2.0;  H[ 1 + 3 * N ] = 1.0; H[ 1 + 4 * N ] = 0.5;
	H[ 2 + 1 * N ] = 2.0; H[ 2 + 2 * N ] = 1.0;  H[ 2 + 3 * N ] = 3.0; H[ 2 + 4 * N ] = 1.0;
	H[ 3 + 2 * N ] = 1.5; H[ 3 + 3 * N ] = 2.0;  H[ 3 + 4 * N ] = 2.0;
	H[ 4 + 3 * N ] = 0.5; H[ 4 + 4 * N ] = 4.0;

	var Z = identityMatrix( N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var info = dlahqr( true, true, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( WR ), tc.wr, 1e-12, 'wr' );
	assertArrayClose( Array.from( WI ), tc.wi, 1e-12, 'wi' );
	assertArrayClose( Array.from( H ), tc.h, 1e-12, 'h' );
	assertArrayClose( Array.from( Z ), tc.z, 1e-12, 'z' );
});

test( 'dlahqr: wantt_no_wantz_3x3', function t() {
	var tc = findCase( 'wantt_no_wantz_3x3' );
	var N = 3;
	// row 0: [2, 1, 0.5]
	// row 1: [3, 1, 2]
	// row 2: [0, 1, 3]
	var H = new Float64Array( [
		2.0, 3.0, 0.0,
		1.0, 1.0, 1.0,
		0.5, 2.0, 3.0
	] );
	var Z = new Float64Array( N * N );
	var WR = new Float64Array( N );
	var WI = new Float64Array( N );
	var info = dlahqr( true, false, N, 1, N, H, 1, N, 0, WR, 1, 0, WI, 1, 0, 1, N, Z, 1, N, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( WR ), tc.wr, 1e-13, 'wr' );
	assertArrayClose( Array.from( WI ), tc.wi, 1e-13, 'wi' );
	assertArrayClose( Array.from( H ), tc.h, 1e-13, 'h' );
});
