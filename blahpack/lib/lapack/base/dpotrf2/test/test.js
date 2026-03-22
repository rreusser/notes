'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dpotrf2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dpotrf2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dpotrf2: lower_3x3', function t() {
	var tc = findCase( 'lower_3x3' );
	// A = [4 2 1; 2 5 3; 1 3 9] col-major
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	var info = dpotrf2( 'L', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.L, 1e-14, 'L' );
});

test( 'dpotrf2: upper_3x3', function t() {
	var tc = findCase( 'upper_3x3' );
	var A = new Float64Array( [ 4, 2, 1, 2, 5, 3, 1, 3, 9 ] );
	var info = dpotrf2( 'U', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.U, 1e-14, 'U' );
});

test( 'dpotrf2: not_posdef', function t() {
	var tc = findCase( 'not_posdef' );
	var A = new Float64Array( [ 1, 2, 3, 2, 1, 4, 3, 4, 1 ] );
	var info = dpotrf2( 'L', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'dpotrf2: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Float64Array( 1 );
	var info = dpotrf2( 'L', 0, A, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dpotrf2: n_one', function t() {
	var tc = findCase( 'n_one' );
	var A = new Float64Array( [ 9 ] );
	var info = dpotrf2( 'L', 1, A, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.a, 1e-14, 'a' );
});

test( 'dpotrf2: n_one_notposdef', function t() {
	var tc = findCase( 'n_one_notposdef' );
	var A = new Float64Array( [ -4 ] );
	var info = dpotrf2( 'L', 1, A, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'dpotrf2: lower_4x4', function t() {
	var tc = findCase( 'lower_4x4' );
	var A = new Float64Array( [ 4, 2, 1, 0, 2, 5, 3, 1, 1, 3, 9, 2, 0, 1, 2, 8 ] );
	var info = dpotrf2( 'L', 4, A, 1, 4, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.L, 1e-14, 'L' );
});

test( 'dpotrf2: upper_4x4', function t() {
	var tc = findCase( 'upper_4x4' );
	var A = new Float64Array( [ 4, 2, 1, 0, 2, 5, 3, 1, 1, 3, 9, 2, 0, 1, 2, 8 ] );
	var info = dpotrf2( 'U', 4, A, 1, 4, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( A ), tc.U, 1e-14, 'U' );
});
