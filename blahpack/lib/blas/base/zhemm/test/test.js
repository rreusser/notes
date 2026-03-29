/* eslint-disable no-restricted-syntax, stdlib/require-globals, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Complex128 = require( '@stdlib/complex/float64/ctor' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zhemm = require( './../lib/base.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zhemm.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zhemm: left_upper_basic', function t() {
	var tc = findCase( 'left_upper_basic' );
	// A is 3x3 Hermitian upper, LDA=3
	// A = [2 1+i 3-2i; * 4 2+i; * * 5]
	var A = new Complex128Array( [
		2, 0, 0, 0, 0, 0,       // col 1: A(1,1)=2, A(2,1)=0, A(3,1)=0
		1, 1, 4, 0, 0, 0,       // col 2: A(1,2)=1+i, A(2,2)=4, A(3,2)=0
		3, -2, 2, 1, 5, 0       // col 3: A(1,3)=3-2i, A(2,3)=2+i, A(3,3)=5
	] );
	var B = new Complex128Array( [
		1, 0.5, 2, -1, 3, 1,    // col 1
		4, 2, 5, 0, 6, -0.5     // col 2
	] );
	var C = new Complex128Array( 6 );

	zhemm( 'left', 'upper', 3, 2, new Complex128( 1, 0 ), A, 1, 3, 0, B, 1, 3, 0, new Complex128( 0, 0 ), C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zhemm: left_lower_basic', function t() {
	var tc = findCase( 'left_lower_basic' );
	// A is 3x3 Hermitian lower
	var A = new Complex128Array( [
		2, 0, 1, -1, 3, 2,      // col 1: A(1,1)=2, A(2,1)=1-i, A(3,1)=3+2i
		0, 0, 4, 0, 2, -1,      // col 2: A(1,2)=0, A(2,2)=4, A(3,2)=2-i
		0, 0, 0, 0, 5, 0        // col 3: A(1,3)=0, A(2,3)=0, A(3,3)=5
	] );
	var B = new Complex128Array( [
		1, 0.5, 2, -1, 3, 1,
		4, 2, 5, 0, 6, -0.5
	] );
	var C = new Complex128Array( 6 );

	zhemm( 'left', 'lower', 3, 2, new Complex128( 1, 0 ), A, 1, 3, 0, B, 1, 3, 0, new Complex128( 0, 0 ), C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zhemm: right_upper_basic', function t() {
	var tc = findCase( 'right_upper_basic' );
	var A = new Complex128Array( [
		2, 0, 0, 0, 0, 0,
		1, 1, 4, 0, 0, 0,
		3, -2, 2, 1, 5, 0
	] );
	// B is 2x3, LDB=2
	var B = new Complex128Array( [
		1, 0.5, 2, -1,
		3, 1, 4, 2,
		5, 0, 6, -0.5
	] );
	var C = new Complex128Array( 6 );

	zhemm( 'right', 'upper', 2, 3, new Complex128( 1, 0 ), A, 1, 3, 0, B, 1, 2, 0, new Complex128( 0, 0 ), C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zhemm: right_lower_basic', function t() {
	var tc = findCase( 'right_lower_basic' );
	var A = new Complex128Array( [
		2, 0, 1, -1, 3, 2,
		0, 0, 4, 0, 2, -1,
		0, 0, 0, 0, 5, 0
	] );
	var B = new Complex128Array( [
		1, 0.5, 2, -1,
		3, 1, 4, 2,
		5, 0, 6, -0.5
	] );
	var C = new Complex128Array( 6 );

	zhemm( 'right', 'lower', 2, 3, new Complex128( 1, 0 ), A, 1, 3, 0, B, 1, 2, 0, new Complex128( 0, 0 ), C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zhemm: complex_alpha_beta', function t() {
	var tc = findCase( 'complex_alpha_beta' );
	var A = new Complex128Array( [
		2, 0, 0, 0, 0, 0,
		1, 1, 4, 0, 0, 0,
		3, -2, 2, 1, 5, 0
	] );
	var B = new Complex128Array( [
		1, 0.5, 2, -1, 3, 1,
		4, 2, 5, 0, 6, -0.5
	] );
	var C = new Complex128Array( [
		1, 1, 2, -1, 0.5, 0.5,
		1, 0, 0, 2, 3, -1
	] );

	zhemm( 'left', 'upper', 3, 2, new Complex128( 2, 1 ), A, 1, 3, 0, B, 1, 3, 0, new Complex128( 0.5, -0.5 ), C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zhemm: alpha_zero', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = new Complex128Array( 9 );
	var B = new Complex128Array( 4 );
	var C = new Complex128Array( [
		1, 2, 3, 4,
		5, 6, 7, 8
	] );

	zhemm( 'left', 'upper', 2, 2, new Complex128( 0, 0 ), A, 1, 3, 0, B, 1, 2, 0, new Complex128( 2, 0 ), C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zhemm: m_zero', function t() {
	var tc = findCase( 'm_zero' );
	var C = new Complex128Array( [ 99, 0 ] );

	zhemm( 'left', 'upper', 0, 2, new Complex128( 1, 0 ), new Complex128Array( 1 ), 1, 1, 0, new Complex128Array( 1 ), 1, 1, 0, new Complex128( 0, 0 ), C, 1, 1, 0 );
	// Should not modify C
	var v = reinterpret( C, 0 );
	assertClose( v[ 0 ], tc.C[ 0 ], 1e-14, 'C[0]' );
	assertClose( v[ 1 ], tc.C[ 1 ], 1e-14, 'C[1]' );
});

test( 'zhemm: n_zero', function t() {
	var tc = findCase( 'n_zero' );
	var C = new Complex128Array( [ 99, 0 ] );

	zhemm( 'left', 'upper', 2, 0, new Complex128( 1, 0 ), new Complex128Array( 4 ), 1, 2, 0, new Complex128Array( 4 ), 1, 2, 0, new Complex128( 0, 0 ), C, 1, 2, 0 );
	var v = reinterpret( C, 0 );
	assertClose( v[ 0 ], tc.C[ 0 ], 1e-14, 'C[0]' );
	assertClose( v[ 1 ], tc.C[ 1 ], 1e-14, 'C[1]' );
});

test( 'zhemm: scalar', function t() {
	var tc = findCase( 'scalar' );
	var A = new Complex128Array( [ 3, 0 ] );
	var B = new Complex128Array( [ 5, 2 ] );
	var C = new Complex128Array( 1 );

	zhemm( 'left', 'upper', 1, 1, new Complex128( 2, 1 ), A, 1, 1, 0, B, 1, 1, 0, new Complex128( 0, 0 ), C, 1, 1, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zhemm: beta_zero', function t() {
	var tc = findCase( 'beta_zero' );
	var A = new Complex128Array( [
		1, 0, 0, 0,
		0, 0, 1, 0
	] );
	var B = new Complex128Array( [
		2, 1, 3, -1,
		4, 0.5, 5, 2
	] );
	var C = new Complex128Array( [
		999, 999, 999, 999,
		999, 999, 999, 999
	] );

	zhemm( 'left', 'lower', 2, 2, new Complex128( 1, 0 ), A, 1, 2, 0, B, 1, 2, 0, new Complex128( 0, 0 ), C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zhemm: alpha_zero_beta_zero (zeros C)', function t() {
	var tc = findCase( 'alpha_zero_beta_zero' );
	var A = new Complex128Array( 4 );
	var B = new Complex128Array( 4 );
	var C = new Complex128Array( [
		99, 88, 77, 66,
		55, 44, 33, 22
	] );

	zhemm( 'left', 'upper', 2, 2, new Complex128( 0, 0 ), A, 1, 2, 0, B, 1, 2, 0, new Complex128( 0, 0 ), C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zhemm: left_lower_nonzero_beta', function t() {
	var tc = findCase( 'left_lower_nonzero_beta' );
	var A = new Complex128Array( [
		2, 0, 1, -1, 3, 2,
		0, 0, 4, 0, 2, -1,
		0, 0, 0, 0, 5, 0
	] );
	var B = new Complex128Array( [
		1, 0.5, 2, -1, 3, 1,
		4, 2, 5, 0, 6, -0.5
	] );
	var C = new Complex128Array( [
		1, 1, 2, -1, 0.5, 0.5,
		1, 0, 0, 2, 3, -1
	] );

	zhemm( 'left', 'lower', 3, 2, new Complex128( 1, 0 ), A, 1, 3, 0, B, 1, 3, 0, new Complex128( 0.5, 0 ), C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});

test( 'zhemm: right_upper_nonzero_beta', function t() {
	var tc = findCase( 'right_upper_nonzero_beta' );
	var A = new Complex128Array( [
		2, 0, 0, 0, 0, 0,
		1, 1, 4, 0, 0, 0,
		3, -2, 2, 1, 5, 0
	] );
	var B = new Complex128Array( [
		1, 0.5, 2, -1,
		3, 1, 4, 2,
		5, 0, 6, -0.5
	] );
	var C = new Complex128Array( [
		1, 1, 2, -1,
		0.5, 0.5, 1, 0,
		0, 2, 3, -1
	] );

	zhemm( 'right', 'upper', 2, 3, new Complex128( 1, 0 ), A, 1, 3, 0, B, 1, 2, 0, new Complex128( 0.5, 0.5 ), C, 1, 2, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.C, 1e-14, 'C' );
});
