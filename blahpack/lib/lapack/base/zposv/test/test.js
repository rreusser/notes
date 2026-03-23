'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zposv = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zposv.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zposv: lower_3x3', function t() {
	var tc = findCase( 'lower_3x3' );
	// Hermitian positive definite 3x3: A = [10 3-i 1+2i; 3+i 8 2-i; 1-2i 2+i 6]
	var A = new Complex128Array( [
		10, 0, 3, 1, 1, -2,
		3, -1, 8, 0, 2, 1,
		1, 2, 2, -1, 6, 0
	] );
	var B = new Complex128Array( [ 1, 1, 2, -1, 3, 0.5 ] );
	var info = zposv( 'lower', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'zposv: upper_3x3', function t() {
	var tc = findCase( 'upper_3x3' );
	var A = new Complex128Array( [
		10, 0, 3, 1, 1, -2,
		3, -1, 8, 0, 2, 1,
		1, 2, 2, -1, 6, 0
	] );
	var B = new Complex128Array( [ 1, 1, 2, -1, 3, 0.5 ] );
	var info = zposv( 'upper', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'zposv: not_posdef', function t() {
	var tc = findCase( 'not_posdef' );
	var A = new Complex128Array( [
		1, 0, 2, 1, 3, 0,
		2, -1, 1, 0, 4, 0,
		3, 0, 4, 0, 1, 0
	] );
	var B = new Complex128Array( [ 1, 0, 1, 0, 1, 0 ] );
	var info = zposv( 'lower', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'zposv: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 1 );
	var info = zposv( 'lower', 0, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zposv: identity', function t() {
	var tc = findCase( 'identity' );
	var A = new Complex128Array( [
		1, 0, 0, 0, 0, 0,
		0, 0, 1, 0, 0, 0,
		0, 0, 0, 0, 1, 0
	] );
	var B = new Complex128Array( [ 3, 1, 5, -2, 7, 0.5 ] );
	var info = zposv( 'lower', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'zposv: multi_rhs', function t() {
	var tc = findCase( 'multi_rhs' );
	var A = new Complex128Array( [
		10, 0, 3, 1, 1, -2,
		3, -1, 8, 0, 2, 1,
		1, 2, 2, -1, 6, 0
	] );
	// B is 3x2 identity (col-major complex)
	var B = new Complex128Array( [
		1, 0, 0, 0, 0, 0,
		0, 0, 1, 0, 0, 0
	] );
	var info = zposv( 'lower', 3, 2, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'zposv: nrhs_zero', function t() {
	var tc = findCase( 'nrhs_zero' );
	var A = new Complex128Array( 9 );
	var B = new Complex128Array( 3 );
	var info = zposv( 'lower', 3, 0, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});
