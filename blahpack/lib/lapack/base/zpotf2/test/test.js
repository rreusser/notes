

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpotf2 = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zpotf2.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zpotf2: upper_2x2', function t() {
	var tc = findCase( 'upper_2x2' );
	// A = [4 (2+i); . 5] HPD upper stored, column-major, LDA=2
	var A = new Complex128Array( [
		4, 0, 0, 0,
		2, 1, 5, 0
	] );
	var info = zpotf2( 'upper', 2, A, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zpotf2: lower_2x2', function t() {
	var tc = findCase( 'lower_2x2' );
	// A = [4 .; (2-i) 5] HPD lower stored, column-major, LDA=2
	var A = new Complex128Array( [
		4, 0, 2, -1,
		0, 0, 5, 0
	] );
	var info = zpotf2( 'lower', 2, A, 1, 2, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zpotf2: n_zero (N=0 quick return)', function t() {
	var A = new Complex128Array( 1 );
	var info = zpotf2( 'upper', 0, A, 1, 1, 0 );
	assert.equal( info, 0 );
});

test( 'zpotf2: n_one (N=1)', function t() {
	var tc = findCase( 'n_one' );
	var A = new Complex128Array( [ 9, 0 ] );
	var info = zpotf2( 'upper', 1, A, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zpotf2: upper_3x3', function t() {
	var tc = findCase( 'upper_3x3' );
	// A = [10 (2+i) (3-2i); . 8 (1+i); . . 6] upper stored, LDA=3
	var A = new Complex128Array( [
		10, 0, 0, 0, 0, 0,
		2, 1, 8, 0, 0, 0,
		3, -2, 1, 1, 6, 0
	] );
	var info = zpotf2( 'upper', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zpotf2: lower_3x3', function t() {
	var tc = findCase( 'lower_3x3' );
	// Same HPD matrix, lower stored
	var A = new Complex128Array( [
		10, 0, 2, -1, 3, 2,
		0, 0, 8, 0, 1, -1,
		0, 0, 0, 0, 6, 0
	] );
	var info = zpotf2( 'lower', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.A, 1e-14, 'A' );
});

test( 'zpotf2: not_hpd (upper, not positive definite)', function t() {
	var tc = findCase( 'not_hpd' );
	var A = new Complex128Array( [
		1, 0, 0, 0,
		2, 1, 1, 0
	] );
	var info = zpotf2( 'upper', 2, A, 1, 2, 0 );
	assert.equal( info, tc.info );
});

test( 'zpotf2: not_hpd_lower (lower, not positive definite)', function t() {
	var tc = findCase( 'not_hpd_lower' );
	var A = new Complex128Array( [
		1, 0, 2, -1,
		0, 0, 1, 0
	] );
	var info = zpotf2( 'lower', 2, A, 1, 2, 0 );
	assert.equal( info, tc.info );
});
