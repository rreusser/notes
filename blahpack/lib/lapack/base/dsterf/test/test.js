'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dsterf = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dsterf.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dsterf: n_zero', function t() {
	var info;
	var d;
	var e;

	d = new Float64Array( 0 );
	e = new Float64Array( 0 );
	info = dsterf( 0, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsterf: n_one', function t() {
	var tc = findCase( 'n_one' );
	var info;
	var d;
	var e;

	d = new Float64Array( [ 5.0 ] );
	e = new Float64Array( 0 );
	info = dsterf( 1, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dsterf: two_by_two', function t() {
	var tc = findCase( 'two_by_two' );
	var info;
	var d;
	var e;

	d = new Float64Array( [ 2.0, 3.0 ] );
	e = new Float64Array( [ 1.0 ] );
	info = dsterf( 2, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dsterf: four_by_four', function t() {
	var tc = findCase( 'four_by_four' );
	var info;
	var d;
	var e;

	d = new Float64Array( [ 4.0, 3.0, 2.0, 1.0 ] );
	e = new Float64Array( [ 1.0, 1.0, 1.0 ] );
	info = dsterf( 4, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dsterf: already_diagonal', function t() {
	var tc = findCase( 'already_diagonal' );
	var info;
	var d;
	var e;

	d = new Float64Array( [ 3.0, 1.0, 4.0, 2.0 ] );
	e = new Float64Array( [ 0.0, 0.0, 0.0 ] );
	info = dsterf( 4, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dsterf: six_by_six_mixed', function t() {
	var tc = findCase( 'six_by_six_mixed' );
	var info;
	var d;
	var e;

	d = new Float64Array( [ -2.0, 1.0, -3.0, 4.0, -1.0, 2.0 ] );
	e = new Float64Array( [ 1.0, 2.0, 1.0, 3.0, 1.0 ] );
	info = dsterf( 6, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dsterf: split_matrix', function t() {
	var tc = findCase( 'split_matrix' );
	var info;
	var d;
	var e;

	d = new Float64Array( [ 2.0, 3.0, 5.0, 7.0 ] );
	e = new Float64Array( [ 1.0, 0.0, 2.0 ] );
	info = dsterf( 4, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dsterf: identity_tridiag', function t() {
	var tc = findCase( 'identity_tridiag' );
	var info;
	var d;
	var e;

	d = new Float64Array( [ 1.0, 1.0, 1.0 ] );
	e = new Float64Array( [ 0.0, 0.0 ] );
	info = dsterf( 3, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dsterf: toeplitz', function t() {
	var tc = findCase( 'toeplitz' );
	var info;
	var d;
	var e;

	d = new Float64Array( [ 2.0, 2.0, 2.0, 2.0, 2.0 ] );
	e = new Float64Array( [ 1.0, 1.0, 1.0, 1.0 ] );
	info = dsterf( 5, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dsterf: eight_by_eight', function t() {
	var tc = findCase( 'eight_by_eight' );
	var info;
	var d;
	var e;

	d = new Float64Array( [ 10.0, 1.0, 8.0, 3.0, 6.0, 5.0, 4.0, 7.0 ] );
	e = new Float64Array( [ 2.0, 3.0, 1.0, 4.0, 2.0, 1.0, 3.0 ] );
	info = dsterf( 8, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dsterf: qr_path', function t() {
	var tc = findCase( 'qr_path' );
	var info;
	var d;
	var e;

	// Small first diagonal, large last diagonal -> exercises QR branch
	d = new Float64Array( [ 0.1, 0.5, 10.0 ] );
	e = new Float64Array( [ 1.0, 1.0 ] );
	info = dsterf( 3, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dsterf: large_values', function t() {
	var tc = findCase( 'large_values' );
	var info;
	var d;
	var e;

	// Very large values to exercise upscaling (iscale = 1)
	// Need anorm > ssfmax ~ 2.23e153
	d = new Float64Array( [ 1.0e154, 2.0e154, 3.0e154 ] );
	e = new Float64Array( [ 0.5e154, 0.5e154 ] );
	info = dsterf( 3, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dsterf: small_values', function t() {
	var tc = findCase( 'small_values' );
	var info;
	var d;
	var e;

	// Very small values to exercise downscaling (iscale = 2)
	d = new Float64Array( [ 1.0e-155, 2.0e-155, 3.0e-155 ] );
	e = new Float64Array( [ 0.5e-155, 0.5e-155 ] );
	info = dsterf( 3, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dsterf: qr_four_by_four', function t() {
	var tc = findCase( 'qr_four_by_four' );
	var info;
	var d;
	var e;

	// 4x4 with increasing diagonal to force QR path
	d = new Float64Array( [ 1.0, 2.0, 3.0, 100.0 ] );
	e = new Float64Array( [ 5.0, 5.0, 5.0 ] );
	info = dsterf( 4, d, 1, 0, e, 1, 0 );
	assert.equal( info, 0, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
});

test( 'dsterf: strided input', function t() {
	var tc = findCase( 'two_by_two' );
	var info;
	var d;
	var e;

	// Use stride 2 and offset 1
	d = new Float64Array( [ 0.0, 2.0, 0.0, 3.0 ] );
	e = new Float64Array( [ 0.0, 1.0 ] );
	info = dsterf( 2, d, 2, 1, e, 1, 1 );
	assert.equal( info, 0, 'info' );
	assertClose( d[ 1 ], tc.d[ 0 ], 1e-14, 'd[0]' );
	assertClose( d[ 3 ], tc.d[ 1 ], 1e-14, 'd[1]' );
});
