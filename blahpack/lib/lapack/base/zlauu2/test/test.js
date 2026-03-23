'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlauu2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zlauu2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zlauu2: upper 3x3', function t() {
	var tc = findCase( 'upper_3x3' );
	// U = [2, 1+0.5i, 3+i; 0, 4, 5+i; 0, 0, 6] (real diagonal)
	var A = new Complex128Array( [
		2, 0,    0, 0,      0, 0,
		1, 0.5,  4, 0,      0, 0,
		3, 1,    5, 1,      6, 0
	] );
	var info = zlauu2( 'upper', 3, A, 1, 3, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauu2: lower 3x3', function t() {
	var tc = findCase( 'lower_3x3' );
	// L = [2, 0, 0; 1+0.5i, 4, 0; 3+i, 5+i, 6] (real diagonal)
	var A = new Complex128Array( [
		2, 0,    1, 0.5,   3, 1,
		0, 0,    4, 0,     5, 1,
		0, 0,    0, 0,     6, 0
	] );
	var info = zlauu2( 'lower', 3, A, 1, 3, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauu2: N=1 upper', function t() {
	var tc = findCase( 'n_one_upper' );
	var A = new Complex128Array( [ 5, 0 ] );
	var info = zlauu2( 'upper', 1, A, 1, 1, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauu2: N=1 lower', function t() {
	var tc = findCase( 'n_one_lower' );
	var A = new Complex128Array( [ 3, 0 ] );
	var info = zlauu2( 'lower', 1, A, 1, 1, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauu2: N=0 quick return', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 1 );
	var info = zlauu2( 'upper', 0, A, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zlauu2: upper 4x4', function t() {
	var tc = findCase( 'upper_4x4' );
	var A = new Complex128Array( [
		1, 0,     0, 0,     0, 0,     0, 0,
		2, 1,     5, 0,     0, 0,     0, 0,
		3, 0,     6, 0.5,   8, 0,     0, 0,
		4, 2,     7, 3,     9, 1,    10, 0
	] );
	var info = zlauu2( 'upper', 4, A, 1, 4, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauu2: lower 4x4', function t() {
	var tc = findCase( 'lower_4x4' );
	var A = new Complex128Array( [
		1, 0,    2, 1,     3, 0,    4, 2,
		0, 0,    5, 0,     6, 0.5,  7, 3,
		0, 0,    0, 0,     8, 0,    9, 1,
		0, 0,    0, 0,     0, 0,   10, 0
	] );
	var info = zlauu2( 'lower', 4, A, 1, 4, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauu2: identity upper', function t() {
	var tc = findCase( 'identity_upper' );
	var A = new Complex128Array( [
		1, 0,  0, 0,  0, 0,
		0, 0,  1, 0,  0, 0,
		0, 0,  0, 0,  1, 0
	] );
	var info = zlauu2( 'upper', 3, A, 1, 3, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauu2: identity lower', function t() {
	var tc = findCase( 'identity_lower' );
	var A = new Complex128Array( [
		1, 0,  0, 0,  0, 0,
		0, 0,  1, 0,  0, 0,
		0, 0,  0, 0,  1, 0
	] );
	var info = zlauu2( 'lower', 3, A, 1, 3, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});
