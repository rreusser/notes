'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlauum = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlauum.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zlauum: upper 3x3', function t() {
	var tc = findCase( 'upper_3x3' );
	// U = [2, 1+0.5i, 3+i; 0, 4, 5+i; 0, 0, 6] (real diagonal)
	var A = new Complex128Array( [
		2, 0,    0, 0,      0, 0,
		1, 0.5,  4, 0,      0, 0,
		3, 1,    5, 1,      6, 0
	] );
	var info = zlauum( 'upper', 3, A, 1, 3, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauum: lower 3x3', function t() {
	var tc = findCase( 'lower_3x3' );
	// L = [2, 0, 0; 1+0.5i, 4, 0; 3+i, 5+i, 6] (real diagonal)
	var A = new Complex128Array( [
		2, 0,    1, 0.5,   3, 1,
		0, 0,    4, 0,     5, 1,
		0, 0,    0, 0,     6, 0
	] );
	var info = zlauum( 'lower', 3, A, 1, 3, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauum: N=1', function t() {
	var tc = findCase( 'n_one' );
	var A = new Complex128Array( [ 5, 0 ] );
	var info = zlauum( 'upper', 1, A, 1, 1, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauum: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 1 );
	var info = zlauum( 'upper', 0, A, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zlauum: upper 4x4', function t() {
	var tc = findCase( 'upper_4x4' );
	var A = new Complex128Array( [
		1, 0,     0, 0,     0, 0,     0, 0,
		2, 1,     5, 0,     0, 0,     0, 0,
		3, 0,     6, 0.5,   8, 0,     0, 0,
		4, 2,     7, 3,     9, 1,    10, 0
	] );
	var info = zlauum( 'upper', 4, A, 1, 4, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauum: lower 4x4', function t() {
	var tc = findCase( 'lower_4x4' );
	var A = new Complex128Array( [
		1, 0,    2, 1,     3, 0,    4, 2,
		0, 0,    5, 0,     6, 0.5,  7, 3,
		0, 0,    0, 0,     8, 0,    9, 1,
		0, 0,    0, 0,     0, 0,   10, 0
	] );
	var info = zlauum( 'lower', 4, A, 1, 4, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauum: identity upper', function t() {
	var tc = findCase( 'identity_upper' );
	var A = new Complex128Array( [
		1, 0,  0, 0,  0, 0,
		0, 0,  1, 0,  0, 0,
		0, 0,  0, 0,  1, 0
	] );
	var info = zlauum( 'upper', 3, A, 1, 3, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});
