'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlauu2 = require( './../lib/ndarray.js' );

// FIXTURES //

var upper_3x3 = require( './fixtures/upper_3x3.json' );
var lower_3x3 = require( './fixtures/lower_3x3.json' );
var n_one_upper = require( './fixtures/n_one_upper.json' );
var n_one_lower = require( './fixtures/n_one_lower.json' );
var n_zero = require( './fixtures/n_zero.json' );
var upper_4x4 = require( './fixtures/upper_4x4.json' );
var lower_4x4 = require( './fixtures/lower_4x4.json' );
var identity_upper = require( './fixtures/identity_upper.json' );
var identity_lower = require( './fixtures/identity_lower.json' );

// FUNCTIONS //

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
	var tc = upper_3x3;
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
	var tc = lower_3x3;
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
	var tc = n_one_upper;
	var A = new Complex128Array( [ 5, 0 ] );
	var info = zlauu2( 'upper', 1, A, 1, 1, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauu2: N=1 lower', function t() {
	var tc = n_one_lower;
	var A = new Complex128Array( [ 3, 0 ] );
	var info = zlauu2( 'lower', 1, A, 1, 1, 0 );
	var view = reinterpret( A, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( view ), tc.a, 1e-14, 'a' );
});

test( 'zlauu2: N=0 quick return', function t() {
	var tc = n_zero;
	var A = new Complex128Array( 1 );
	var info = zlauu2( 'upper', 0, A, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zlauu2: upper 4x4', function t() {
	var tc = upper_4x4;
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
	var tc = lower_4x4;
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
	var tc = identity_upper;
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
	var tc = identity_lower;
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
