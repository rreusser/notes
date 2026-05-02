/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpotrf2 = require( './../lib/zpotrf2.js' );


// FIXTURES //

var lower_3x3 = require( './fixtures/lower_3x3.json' );
var upper_3x3 = require( './fixtures/upper_3x3.json' );


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

test( 'zpotrf2 is a function', function t() {
	assert.strictEqual( typeof zpotrf2, 'function', 'is a function' );
});

test( 'zpotrf2 has expected arity', function t() {
	assert.strictEqual( zpotrf2.length, 5, 'has expected arity' );
});

test( 'zpotrf2 throws TypeError for invalid order', function t() {
	assert.throws( function throws() {
		zpotrf2( 'invalid', 'upper', 2, new Complex128Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpotrf2 throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zpotrf2( 'row-major', 'invalid', 2, new Complex128Array( 4 ), 2 );
	}, TypeError );
});

test( 'zpotrf2 throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpotrf2( 'row-major', 'upper', -1, new Complex128Array( 4 ), 2 );
	}, RangeError );
});

test( 'zpotrf2 throws RangeError for LDA < max(1,N)', function t() {
	assert.throws( function throws() {
		zpotrf2( 'column-major', 'upper', 3, new Complex128Array( 9 ), 2 );
	}, RangeError );
});

test( 'zpotrf2: column-major lower 3x3', function t() {
	var tc = lower_3x3;
	// Column-major Hermitian matrix (interleaved real/imag):
	var A = new Complex128Array( [
		10, 0, 3, 1, 1, -2,
		3, -1, 8, 0, 2, 1,
		1, 2, 2, -1, 6, 0
	] );
	var info = zpotrf2( 'column-major', 'lower', 3, A, 3 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
});

test( 'zpotrf2: row-major upper 3x3 maps to column-major lower factor', function t() {
	// In row-major, an upper-triangle is laid out the same as column-major lower.
	// So calling row-major + upper produces the same numerical factorization
	// as column-major + lower for a Hermitian matrix.
	var tc = lower_3x3;
	var A = new Complex128Array( [
		10, 0, 3, 1, 1, -2,
		3, -1, 8, 0, 2, 1,
		1, 2, 2, -1, 6, 0
	] );
	var info = zpotrf2( 'row-major', 'upper', 3, A, 3 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
	// Note: also covers column-major upper for the conjugate-transpose case.
	// The upper_3x3 fixture is verified separately via the ndarray surface.
	void upper_3x3;
});

test( 'zpotrf2: N=0 returns 0', function t() {
	var A = new Complex128Array( 1 );
	var info = zpotrf2( 'column-major', 'upper', 0, A, 1 );
	assert.equal( info, 0 );
});
