'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpotrf = require( './../../zpotrf/lib/base.js' );
var zpotrs = require( './../lib/ndarray.js' );
var ndarray = require( './../lib/ndarray.js' );

// FIXTURES //

var lower_single_rhs = require( './fixtures/lower_single_rhs.json' );
var upper_single_rhs = require( './fixtures/upper_single_rhs.json' );
var lower_multi_rhs = require( './fixtures/lower_multi_rhs.json' );
var n_zero = require( './fixtures/n_zero.json' );
var nrhs_zero = require( './fixtures/nrhs_zero.json' );
var one_by_one = require( './fixtures/one_by_one.json' );
var upper_multi_rhs_3 = require( './fixtures/upper_multi_rhs_3.json' );

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

test( 'zpotrs: lower_single_rhs', function t() {
	var tc = lower_single_rhs;
	// Hermitian positive definite 3x3: A = [10 3-i 1+2i; 3+i 8 2-i; 1-2i 2+i 6]
	var A = new Complex128Array( [
		10, 0, 3, 1, 1, -2,
		3, -1, 8, 0, 2, 1,
		1, 2, 2, -1, 6, 0
	] );
	var B = new Complex128Array( [ 1, 1, 2, -1, 3, 0.5 ] );
	zpotrf( 'lower', 3, A, 1, 3, 0 );
	var info = zpotrs( 'lower', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'zpotrs: upper_single_rhs', function t() {
	var tc = upper_single_rhs;
	var A = new Complex128Array( [
		10, 0, 3, 1, 1, -2,
		3, -1, 8, 0, 2, 1,
		1, 2, 2, -1, 6, 0
	] );
	var B = new Complex128Array( [ 1, 1, 2, -1, 3, 0.5 ] );
	zpotrf( 'upper', 3, A, 1, 3, 0 );
	var info = zpotrs( 'upper', 3, 1, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'zpotrs: lower_multi_rhs', function t() {
	var tc = lower_multi_rhs;
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
	zpotrf( 'lower', 3, A, 1, 3, 0 );
	var info = zpotrs( 'lower', 3, 2, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'zpotrs: n_zero', function t() {
	var tc = n_zero;
	var A = new Complex128Array( 1 );
	var B = new Complex128Array( 1 );
	var info = zpotrs( 'lower', 0, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
});

test( 'zpotrs: nrhs_zero', function t() {
	var tc = nrhs_zero;
	var A = new Complex128Array( 9 );
	var B = new Complex128Array( 3 );
	var info = zpotrs( 'lower', 3, 0, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
});

test( 'zpotrs: one_by_one', function t() {
	var tc = one_by_one;
	// L = (2,0), so A = L*L^H = (4,0). Pre-factored: L(1,1) = (2,0)
	var A = new Complex128Array( [ 2, 0 ] );
	var B = new Complex128Array( [ 6, 3 ] );
	var info = zpotrs( 'lower', 1, 1, A, 1, 1, 0, B, 1, 1, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );
});

test( 'zpotrs: upper_multi_rhs_3', function t() {
	var tc = upper_multi_rhs_3;
	var A = new Complex128Array( [
		10, 0, 3, 1, 1, -2,
		3, -1, 8, 0, 2, 1,
		1, 2, 2, -1, 6, 0
	] );
	// B = I (3x3 complex identity, col-major)
	var B = new Complex128Array( [
		1, 0, 0, 0, 0, 0,
		0, 0, 1, 0, 0, 0,
		0, 0, 0, 0, 1, 0
	] );
	zpotrf( 'upper', 3, A, 1, 3, 0 );
	var info = zpotrs( 'upper', 3, 3, A, 1, 3, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.x, 1e-14, 'x' );
});

// ndarray validation tests

test( 'zpotrs: ndarray throws TypeError for invalid uplo', function t() {
	assert.throws( function() {
		ndarray( 'invalid', 3, 1, new Complex128Array( 9 ), 1, 3, 0, new Complex128Array( 3 ), 1, 3, 0 );
	}, TypeError );
});

test( 'zpotrs: ndarray throws RangeError for negative N', function t() {
	assert.throws( function() {
		ndarray( 'upper', -1, 1, new Complex128Array( 9 ), 1, 3, 0, new Complex128Array( 3 ), 1, 3, 0 );
	}, RangeError );
});

test( 'zpotrs: ndarray throws RangeError for negative NRHS', function t() {
	assert.throws( function() {
		ndarray( 'upper', 3, -1, new Complex128Array( 9 ), 1, 3, 0, new Complex128Array( 3 ), 1, 3, 0 );
	}, RangeError );
});
