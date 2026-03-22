

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zgelq2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zgelq2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zgelq2: basic 2x3 matrix', function t() {
	var tc = findCase( 'basic_2x3' );
	var a = new Complex128Array( [
		1, 0, 4, 1,
		2, 1, 5, 0,
		3, 0, 6, -1
	] );
	var tau = new Complex128Array( 2 );
	var work = new Complex128Array( 10 );
	var info = zgelq2( 2, 3, a, 1, 2, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgelq2: basic 3x4 matrix', function t() {
	var tc = findCase( 'basic_3x4' );
	var a = new Complex128Array( [
		1, 1, 5, 0, 9, 1,
		2, 0, 6, 1, 10, 0,
		3, -1, 7, 0, 11, 1,
		4, 0, 8, -1, 12, 0
	] );
	var tau = new Complex128Array( 3 );
	var work = new Complex128Array( 10 );
	var info = zgelq2( 3, 4, a, 1, 3, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgelq2: M=0 quick return', function t() {
	var a = new Complex128Array( 2 );
	var tau = new Complex128Array( 2 );
	var work = new Complex128Array( 2 );
	var info = zgelq2( 0, 3, a, 1, 1, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, 0, 1e-14, 'info' );
});

test( 'zgelq2: N=0 quick return', function t() {
	var a = new Complex128Array( 6 );
	var tau = new Complex128Array( 2 );
	var work = new Complex128Array( 2 );
	var info = zgelq2( 2, 0, a, 1, 2, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, 0, 1e-14, 'info' );
});

test( 'zgelq2: 1x1 matrix', function t() {
	var tc = findCase( 'one_by_one' );
	var a = new Complex128Array( [ 5, 3 ] );
	var tau = new Complex128Array( 1 );
	var work = new Complex128Array( 2 );
	var info = zgelq2( 1, 1, a, 1, 1, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgelq2: square 2x2 matrix', function t() {
	var tc = findCase( 'square_2x2' );
	var a = new Complex128Array( [ 1, 1, 0, 1, 1, 0, 1, 1 ] );
	var tau = new Complex128Array( 2 );
	var work = new Complex128Array( 10 );
	var info = zgelq2( 2, 2, a, 1, 2, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});

test( 'zgelq2: square 3x3 matrix', function t() {
	var tc = findCase( 'square_3x3' );
	var a = new Complex128Array( [
		2, 1, 1, -1, 0, 0.5,
		1, 0, 3, 2, 1, -1,
		-1, 1, 0, 1, 4, 0
	] );
	var tau = new Complex128Array( 3 );
	var work = new Complex128Array( 10 );
	var info = zgelq2( 3, 3, a, 1, 3, 0, tau, 1, 0, work, 1, 0 );
	assertClose( info, tc.info, 1e-14, 'info' );
	assertArrayClose( reinterpret( a, 0 ), tc.a, 1e-10, 'a' );
	assertArrayClose( reinterpret( tau, 0 ), tc.tau, 1e-10, 'tau' );
});
