'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpotrf2 = require( './../lib/base.js' );

// FIXTURES //

var lower_3x3 = require( './fixtures/lower_3x3.json' );
var upper_3x3 = require( './fixtures/upper_3x3.json' );
var not_posdef = require( './fixtures/not_posdef.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var n_one_notposdef = require( './fixtures/n_one_notposdef.json' );
var lower_4x4 = require( './fixtures/lower_4x4.json' );
var upper_4x4 = require( './fixtures/upper_4x4.json' );

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

test( 'zpotrf2: lower_3x3', function t() {
	var tc = lower_3x3;
	// A = [10 3-i 1+2i; 3+i 8 2-i; 1-2i 2+i 6] (col-major interleaved)
	var A = new Complex128Array( [
		10, 0, 3, 1, 1, -2,
		3, -1, 8, 0, 2, 1,
		1, 2, 2, -1, 6, 0
	] );
	var info = zpotrf2( 'lower', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
});

test( 'zpotrf2: upper_3x3', function t() {
	var tc = upper_3x3;
	var A = new Complex128Array( [
		10, 0, 3, 1, 1, -2,
		3, -1, 8, 0, 2, 1,
		1, 2, 2, -1, 6, 0
	] );
	var info = zpotrf2( 'upper', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
});

test( 'zpotrf2: not_posdef', function t() {
	var tc = not_posdef;
	var A = new Complex128Array( [
		1, 0, 2, 1, 3, 0,
		2, -1, 1, 0, 4, 0,
		3, 0, 4, 0, 1, 0
	] );
	var info = zpotrf2( 'lower', 3, A, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'zpotrf2: n_zero', function t() {
	var tc = n_zero;
	var A = new Complex128Array( 1 );
	var info = zpotrf2( 'lower', 0, A, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zpotrf2: n_one', function t() {
	var tc = n_one;
	var A = new Complex128Array( [ 9, 0 ] );
	var info = zpotrf2( 'lower', 1, A, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
});

test( 'zpotrf2: n_one_notposdef', function t() {
	var tc = n_one_notposdef;
	var A = new Complex128Array( [ -4, 0 ] );
	var info = zpotrf2( 'lower', 1, A, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zpotrf2: lower_4x4', function t() {
	var tc = lower_4x4;
	var A = new Complex128Array( [
		14, 0, 4, 2, 2, -1, 1, 3,
		4, -2, 12, 0, 3, 1, 2, -2,
		2, 1, 3, -1, 10, 0, 1, 1,
		1, -3, 2, 2, 1, -1, 9, 0
	] );
	var info = zpotrf2( 'lower', 4, A, 1, 4, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
});

test( 'zpotrf2: upper_4x4', function t() {
	var tc = upper_4x4;
	var A = new Complex128Array( [
		14, 0, 4, 2, 2, -1, 1, 3,
		4, -2, 12, 0, 3, 1, 2, -2,
		2, 1, 3, -1, 10, 0, 1, 1,
		1, -3, 2, 2, 1, -1, 9, 0
	] );
	var info = zpotrf2( 'upper', 4, A, 1, 4, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
});

test( 'zpotrf2: upper_4x4 not posdef in A22 block', function t() {
	// 4x4 matrix where upper-left 2x2 is HPD but A22 block fails.
	var A = new Complex128Array( [
		4, 0, 2, 1, 1, 0, 1, 0,
		2, -1, 5, 0, 3, 0, 3, 0,
		1, 0, 3, 0, -1, 0, 0, 0,
		1, 0, 3, 0, 0, 0, -1, 0
	] );
	var info = zpotrf2( 'upper', 4, A, 1, 4, 0 );
	// n1 = 2, so if A22 fails at position k, info = k + n1
	assert.ok( info > 2, 'info should be > 2 (failure in A22 block), got ' + info );
});
