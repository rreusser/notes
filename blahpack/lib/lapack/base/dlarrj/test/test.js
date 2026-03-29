

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var Float64Array = require( '@stdlib/array/float64' );
var Int32Array = require( '@stdlib/array/int32' );
var path = require( 'path' );
var dlarrj = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dlarrj.jsonl' ), 'utf8' ).trim().split( '\n' );
var fixture = lines.map( function parse( line ) { return JSON.parse( line ); } );


// CONSTANTS //

var PIVMIN = 2.2250738585072014e-308; // Number.MIN_VALUE (DLAMCH('S'))


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


// TESTS //

test( 'dlarrj is a function', function t() {
	assert.equal( typeof dlarrj, 'function' );
});

test( 'dlarrj: diagonal_4x4', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = findCase( 'diagonal_4x4' );

	w = new Float64Array( [ 1.1, 2.9, 5.2, 6.8 ] );
	WERR = new Float64Array( [ 0.5, 0.5, 0.5, 0.5 ] );
	WORK = new Float64Array( 16 );
	IWORK = new Int32Array( 16 );

	info = dlarrj(
		4,
		new Float64Array( [ 1.0, 3.0, 5.0, 7.0 ] ), 1, 0,
		new Float64Array( [ 0.0, 0.0, 0.0 ] ), 1, 0,
		1, 4, 1e-14, 0,
		w, 1, 0,
		WERR, 1, 0,
		WORK, 1, 0,
		IWORK, 1, 0,
		PIVMIN, 6.0
	);

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( Array.from( WERR ), tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: tridiag_3x3', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = findCase( 'tridiag_3x3' );

	w = new Float64Array( [ 1.1, 2.1, 3.9 ] );
	WERR = new Float64Array( [ 0.5, 0.5, 0.5 ] );
	WORK = new Float64Array( 12 );
	IWORK = new Int32Array( 12 );

	info = dlarrj(
		3,
		new Float64Array( [ 2.0, 3.0, 2.0 ] ), 1, 0,
		new Float64Array( [ 1.0, 1.0 ] ), 1, 0,
		1, 3, 1e-14, 0,
		w, 1, 0,
		WERR, 1, 0,
		WORK, 1, 0,
		IWORK, 1, 0,
		PIVMIN, 3.0
	);

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( Array.from( WERR ), tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: n_zero', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var w;

	w = new Float64Array( 1 );
	WERR = new Float64Array( 1 );
	WORK = new Float64Array( 4 );
	IWORK = new Int32Array( 4 );

	info = dlarrj(
		0,
		new Float64Array( 1 ), 1, 0,
		new Float64Array( 1 ), 1, 0,
		1, 0, 1e-14, 0,
		w, 1, 0,
		WERR, 1, 0,
		WORK, 1, 0,
		IWORK, 1, 0,
		PIVMIN, 1.0
	);

	assert.equal( info, 0 );
});

test( 'dlarrj: n_one', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = findCase( 'n_one' );

	w = new Float64Array( [ 5.1 ] );
	WERR = new Float64Array( [ 0.5 ] );
	WORK = new Float64Array( 4 );
	IWORK = new Int32Array( 4 );

	info = dlarrj(
		1,
		new Float64Array( [ 5.0 ] ), 1, 0,
		new Float64Array( 1 ), 1, 0,
		1, 1, 1e-14, 0,
		w, 1, 0,
		WERR, 1, 0,
		WORK, 1, 0,
		IWORK, 1, 0,
		PIVMIN, 1.0
	);

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( Array.from( WERR ), tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: subset_refinement', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = findCase( 'subset_refinement' );

	// W and WERR are accessed as W(I-OFFSET), i.e. W(2) and W(3) in Fortran
	// In JS 0-based: w[1] and w[2]
	w = new Float64Array( [ 0.0, 3.1, 4.9, 0.0 ] );
	WERR = new Float64Array( [ 0.0, 0.5, 0.5, 0.0 ] );
	WORK = new Float64Array( 16 );
	IWORK = new Int32Array( 16 );

	info = dlarrj(
		4,
		new Float64Array( [ 1.0, 3.0, 5.0, 7.0 ] ), 1, 0,
		new Float64Array( [ 0.0, 0.0, 0.0 ] ), 1, 0,
		2, 3, 1e-14, 0,
		w, 1, 0,
		WERR, 1, 0,
		WORK, 1, 0,
		IWORK, 1, 0,
		PIVMIN, 6.0
	);

	assert.equal( info, tc.info );

	// Fixture contains W(2:3) in Fortran = w[1] and w[2] in JS
	assertArrayClose( [ w[ 1 ], w[ 2 ] ], tc.w, 1e-12, 'w' );
	assertArrayClose( [ WERR[ 1 ], WERR[ 2 ] ], tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: with_offset', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = findCase( 'with_offset' );

	// IFIRST=3, ILAST=4, OFFSET=2
	// W(I-OFFSET): W(3-2)=W(1), W(4-2)=W(2) in Fortran
	// In JS 0-based: w[0] and w[1]
	w = new Float64Array( [ 5.1, 6.9 ] );
	WERR = new Float64Array( [ 0.5, 0.5 ] );
	WORK = new Float64Array( 16 );
	IWORK = new Int32Array( 16 );

	info = dlarrj(
		4,
		new Float64Array( [ 1.0, 3.0, 5.0, 7.0 ] ), 1, 0,
		new Float64Array( [ 0.0, 0.0, 0.0 ] ), 1, 0,
		3, 4, 1e-14, 2,
		w, 1, 0,
		WERR, 1, 0,
		WORK, 1, 0,
		IWORK, 1, 0,
		PIVMIN, 6.0
	);

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( Array.from( WERR ), tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: already_converged', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = findCase( 'already_converged' );

	// Very tight initial approximations - should be already converged
	w = new Float64Array( [ 1.0, 2.0, 4.0 ] );
	WERR = new Float64Array( [ 1e-16, 1e-16, 1e-16 ] );
	WORK = new Float64Array( 12 );
	IWORK = new Int32Array( 12 );

	info = dlarrj(
		3,
		new Float64Array( [ 2.0, 3.0, 2.0 ] ), 1, 0,
		new Float64Array( [ 1.0, 1.0 ] ), 1, 0,
		1, 3, 1e-14, 0,
		w, 1, 0,
		WERR, 1, 0,
		WORK, 1, 0,
		IWORK, 1, 0,
		PIVMIN, 3.0
	);

	assert.equal( info, tc.info );

	// For already converged intervals, W and WERR should be unchanged
	assertArrayClose( Array.from( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( Array.from( WERR ), tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: tridiag_5x5', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = findCase( 'tridiag_5x5' );

	w = new Float64Array( [ 1.0, 2.5, 4.0, 5.5, 7.0 ] );
	WERR = new Float64Array( [ 1.0, 1.0, 1.0, 1.0, 1.0 ] );
	WORK = new Float64Array( 20 );
	IWORK = new Int32Array( 20 );

	info = dlarrj(
		5,
		new Float64Array( [ 4.0, 3.0, 2.0, 5.0, 6.0 ] ), 1, 0,
		new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] ), 1, 0,
		1, 5, 1e-14, 0,
		w, 1, 0,
		WERR, 1, 0,
		WORK, 1, 0,
		IWORK, 1, 0,
		PIVMIN, 6.0
	);

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( Array.from( WERR ), tc.werr, 1e-6, 'werr' );
});

test( 'dlarrj: left boundary expansion needed', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var w;

	// T = diag([1,3,5]), eigenvalues: 1, 3, 5
	// Provide an approximation for eigenvalue 1 that is too far right,
	// so the left boundary needs expansion.
	// W[0] = 1.5, WERR[0] = 0.1 => left = 1.4, right = 1.6
	// The eigenvalue 1.0 is NOT in [1.4, 1.6], so left must be expanded.
	w = new Float64Array( [ 1.5, 3.0, 5.0 ] );
	WERR = new Float64Array( [ 0.1, 0.5, 0.5 ] );
	WORK = new Float64Array( 12 );
	IWORK = new Int32Array( 12 );

	info = dlarrj(
		3,
		new Float64Array( [ 1.0, 3.0, 5.0 ] ), 1, 0,
		new Float64Array( [ 0.0, 0.0 ] ), 1, 0,
		1, 3, 1e-14, 0,
		w, 1, 0,
		WERR, 1, 0,
		WORK, 1, 0,
		IWORK, 1, 0,
		PIVMIN, 4.0
	);

	assert.equal( info, 0 );

	// The refined eigenvalues should be close to true values:
	assertClose( w[ 0 ], 1.0, 1e-10, 'w[0]' );
	assertClose( w[ 1 ], 3.0, 1e-10, 'w[1]' );
	assertClose( w[ 2 ], 5.0, 1e-10, 'w[2]' );
});

test( 'dlarrj: right boundary expansion needed', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var w;

	// T = diag([1,3,5]), eigenvalues: 1, 3, 5
	// Provide an approximation for eigenvalue 5 that is too far left,
	// so the right boundary needs expansion.
	// W[2] = 4.5, WERR[2] = 0.1 => left = 4.4, right = 4.6
	// The eigenvalue 5.0 is NOT in [4.4, 4.6], so right must be expanded.
	w = new Float64Array( [ 1.0, 3.0, 4.5 ] );
	WERR = new Float64Array( [ 0.5, 0.5, 0.1 ] );
	WORK = new Float64Array( 12 );
	IWORK = new Int32Array( 12 );

	info = dlarrj(
		3,
		new Float64Array( [ 1.0, 3.0, 5.0 ] ), 1, 0,
		new Float64Array( [ 0.0, 0.0 ] ), 1, 0,
		1, 3, 1e-14, 0,
		w, 1, 0,
		WERR, 1, 0,
		WORK, 1, 0,
		IWORK, 1, 0,
		PIVMIN, 4.0
	);

	assert.equal( info, 0 );

	// The refined eigenvalues should be close to true values:
	assertClose( w[ 0 ], 1.0, 1e-10, 'w[0]' );
	assertClose( w[ 1 ], 3.0, 1e-10, 'w[1]' );
	assertClose( w[ 2 ], 5.0, 1e-10, 'w[2]' );
});

test( 'dlarrj: mixed converged and unconverged', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var w;

	// T = diag([1,3,5,7]), eigenvalues: 1, 3, 5, 7
	// Make eigenvalue 1 already converged (tiny WERR), eigenvalue 2 unconverged,
	// then eigenvalue 3 already converged, eigenvalue 4 unconverged.
	// This tests the linked list pointers when converged intervals appear
	// between unconverged ones during initialization.
	w = new Float64Array( [ 1.0, 3.1, 5.0, 6.8 ] );
	WERR = new Float64Array( [ 1e-16, 0.5, 1e-16, 0.5 ] );
	WORK = new Float64Array( 16 );
	IWORK = new Int32Array( 16 );

	info = dlarrj(
		4,
		new Float64Array( [ 1.0, 3.0, 5.0, 7.0 ] ), 1, 0,
		new Float64Array( [ 0.0, 0.0, 0.0 ] ), 1, 0,
		1, 4, 1e-14, 0,
		w, 1, 0,
		WERR, 1, 0,
		WORK, 1, 0,
		IWORK, 1, 0,
		PIVMIN, 6.0
	);

	assert.equal( info, 0 );

	// Already-converged eigenvalues should be unchanged; others refined:
	assertClose( w[ 0 ], 1.0, 1e-12, 'w[0] (already converged)' );
	assertClose( w[ 1 ], 3.0, 1e-10, 'w[1] (refined)' );
	assertClose( w[ 2 ], 5.0, 1e-12, 'w[2] (already converged)' );
	assertClose( w[ 3 ], 7.0, 1e-10, 'w[3] (refined)' );
});

test( 'dlarrj: coarse_rtol', function t() {
	var IWORK;
	var WORK;
	var WERR;
	var info;
	var tc;
	var w;

	tc = findCase( 'coarse_rtol' );

	w = new Float64Array( [ 1.1, 2.1, 3.9 ] );
	WERR = new Float64Array( [ 0.5, 0.5, 0.5 ] );
	WORK = new Float64Array( 12 );
	IWORK = new Int32Array( 12 );

	info = dlarrj(
		3,
		new Float64Array( [ 2.0, 3.0, 2.0 ] ), 1, 0,
		new Float64Array( [ 1.0, 1.0 ] ), 1, 0,
		1, 3, 1e-4, 0,
		w, 1, 0,
		WERR, 1, 0,
		WORK, 1, 0,
		IWORK, 1, 0,
		PIVMIN, 3.0
	);

	assert.equal( info, tc.info );
	assertArrayClose( Array.from( w ), tc.w, 1e-12, 'w' );
	assertArrayClose( Array.from( WERR ), tc.werr, 1e-6, 'werr' );
});
