'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtrtri = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtrtri.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dtrtri: upper, non-unit, 3x3', function t() {
	var tc = findCase( 'upper_nonunit_3' );
	var A = new Float64Array( [ 2, 0, 0, 1, 4, 0, 3, 5, 6 ] );
	var info = dtrtri( 'U', 'N', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dtrtri: lower, non-unit, 3x3', function t() {
	var tc = findCase( 'lower_nonunit_3' );
	var A = new Float64Array( [ 2, 1, 3, 0, 4, 5, 0, 0, 6 ] );
	var info = dtrtri( 'L', 'N', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dtrtri: upper, non-unit, 4x4', function t() {
	var tc = findCase( 'upper_nonunit_4' );
	// A = [1 2 3 4; 0 5 6 7; 0 0 8 9; 0 0 0 10] col-major
	var A = new Float64Array( [
		1, 0, 0, 0,
		2, 5, 0, 0,
		3, 6, 8, 0,
		4, 7, 9, 10
	] );
	var info = dtrtri( 'U', 'N', 4, A, 1, 4, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dtrtri: lower, non-unit, 4x4', function t() {
	var tc = findCase( 'lower_nonunit_4' );
	// A = [1 0 0 0; 2 5 0 0; 3 6 8 0; 4 7 9 10] col-major
	var A = new Float64Array( [
		1, 2, 3, 4,
		0, 5, 6, 7,
		0, 0, 8, 9,
		0, 0, 0, 10
	] );
	var info = dtrtri( 'L', 'N', 4, A, 1, 4, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dtrtri: N=0', function t() {
	var info = dtrtri( 'U', 'N', 0, new Float64Array( 0 ), 1, 1, 0 );
	assert.equal( info, 0 );
});

test( 'dtrtri: singular (zero diagonal)', function t() {
	var tc = findCase( 'singular' );
	// A = [2 3 0; 0 0 0; 0 0 6] — zero at (1,1) in 0-based
	var A = new Float64Array( [ 2, 0, 0, 3, 0, 0, 0, 0, 6 ] );
	var info = dtrtri( 'U', 'N', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'dtrtri: upper, unit diag, 3x3', function t() {
	var tc = findCase( 'upper_unit' );
	var A = new Float64Array( [ 99, 0, 0, 1, 99, 0, 3, 5, 99 ] );
	var info = dtrtri( 'U', 'U', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dtrtri: identity 3x3', function t() {
	var tc = findCase( 'identity' );
	var A = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	var info = dtrtri( 'U', 'N', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dtrtri: N=1 (unblocked path)', function t() {
	var A = new Float64Array( [ 4 ] );
	var info = dtrtri( 'U', 'N', 1, A, 1, 1, 0 );
	assert.equal( info, 0 );
	assertClose( A[ 0 ], 0.25, 1e-14, 'a[0]' );
});

test( 'dtrtri: upper 5x5', function t() {
	var tc = findCase( 'upper_5x5' );
	// A upper 5x5 col-major
	var A = new Float64Array( [
		2, 0, 0, 0, 0,
		1, 4, 0, 0, 0,
		3, 1, 5, 0, 0,
		2, 3, 1, 6, 0,
		1, 2, 4, 1, 3
	] );
	var info = dtrtri( 'U', 'N', 5, A, 1, 5, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dtrtri: lower 5x5', function t() {
	var tc = findCase( 'lower_5x5' );
	// A lower 5x5 col-major
	var A = new Float64Array( [
		2, 1, 3, 2, 1,
		0, 4, 1, 3, 2,
		0, 0, 5, 1, 4,
		0, 0, 0, 6, 1,
		0, 0, 0, 0, 3
	] );
	var info = dtrtri( 'L', 'N', 5, A, 1, 5, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});
