'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var dtrti2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'dtrti2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'dtrti2: upper, non-unit, 3x3', function t() {
	var tc = findCase( 'upper_nonunit' );
	// A = [2 1 3; 0 4 5; 0 0 6] (col-major)
	var A = new Float64Array( [ 2, 0, 0, 1, 4, 0, 3, 5, 6 ] );
	var info = dtrti2( 'upper', 'non-unit', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dtrti2: lower, non-unit, 3x3', function t() {
	var tc = findCase( 'lower_nonunit' );
	// A = [2 0 0; 1 4 0; 3 5 6] (col-major)
	var A = new Float64Array( [ 2, 1, 3, 0, 4, 5, 0, 0, 6 ] );
	var info = dtrti2( 'lower', 'non-unit', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dtrti2: upper, unit diag, 3x3', function t() {
	var tc = findCase( 'upper_unit' );
	// A = [99 1 3; 0 99 5; 0 0 99] (diag ignored with unit diag)
	var A = new Float64Array( [ 99, 0, 0, 1, 99, 0, 3, 5, 99 ] );
	var info = dtrti2( 'upper', 'unit', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dtrti2: lower, unit diag, 3x3', function t() {
	var tc = findCase( 'lower_unit' );
	var A = new Float64Array( [ 99, 1, 3, 0, 99, 5, 0, 0, 99 ] );
	var info = dtrti2( 'lower', 'unit', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dtrti2: N=0', function t() {
	var info = dtrti2( 'upper', 'non-unit', 0, new Float64Array( 0 ), 1, 1, 0 );
	assert.equal( info, 0 );
});

test( 'dtrti2: N=1', function t() {
	var tc = findCase( 'n_one' );
	var A = new Float64Array( [ 4 ] );
	var info = dtrti2( 'upper', 'non-unit', 1, A, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});

test( 'dtrti2: identity 3x3', function t() {
	var tc = findCase( 'identity' );
	var A = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	var info = dtrti2( 'upper', 'non-unit', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( A, tc.a, 1e-14, 'a' );
});
