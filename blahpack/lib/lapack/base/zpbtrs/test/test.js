

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zpbtrf = require( '../../zpbtrf/lib/base.js' );
var zpbtrs = require( './../lib/base.js' );

// FIXTURES //

var upper_single_rhs = require( './fixtures/upper_single_rhs.json' );
var lower_single_rhs = require( './fixtures/lower_single_rhs.json' );
var upper_two_rhs = require( './fixtures/upper_two_rhs.json' );
var lower_two_rhs = require( './fixtures/lower_two_rhs.json' );

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

test( 'zpbtrs: upper_single_rhs (UPLO=U, N=3, KD=1, NRHS=1)', function t() {
	var tc = upper_single_rhs;
	// Factor the band matrix first
	var AB = new Complex128Array( [
		0, 0, 4, 0,
		1, 1, 5, 0,
		2, -1, 6, 0
	] );
	var finfo = zpbtrf( 'upper', 3, 1, AB, 1, 2, 0 );
	assert.equal( finfo, 0 );
	// b = A*[1, 1+i, 2]
	var B = new Complex128Array( [ 4, 2, 10, 2, 13, 3 ] );
	var info = zpbtrs( 'upper', 3, 1, 1, AB, 1, 2, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.B, 1e-13, 'B' );
});

test( 'zpbtrs: lower_single_rhs (UPLO=L, N=3, KD=1, NRHS=1)', function t() {
	var tc = lower_single_rhs;
	var AB = new Complex128Array( [
		4, 0, 1, -1,
		5, 0, 2, 1,
		6, 0, 0, 0
	] );
	var finfo = zpbtrf( 'lower', 3, 1, AB, 1, 2, 0 );
	assert.equal( finfo, 0 );
	var B = new Complex128Array( [ 4, 2, 10, 2, 13, 3 ] );
	var info = zpbtrs( 'lower', 3, 1, 1, AB, 1, 2, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.B, 1e-13, 'B' );
});

test( 'zpbtrs: n_zero (N=0 quick return)', function t() {
	var AB = new Complex128Array( 4 );
	var B = new Complex128Array( 1 );
	var info = zpbtrs( 'upper', 0, 1, 1, AB, 1, 2, 0, B, 1, 1, 0 );
	assert.equal( info, 0 );
});

test( 'zpbtrs: nrhs_zero (NRHS=0 quick return)', function t() {
	var AB = new Complex128Array( 4 );
	var B = new Complex128Array( 3 );
	var info = zpbtrs( 'upper', 3, 1, 0, AB, 1, 2, 0, B, 1, 3, 0 );
	assert.equal( info, 0 );
});

test( 'zpbtrs: upper_two_rhs (UPLO=U, N=3, KD=1, NRHS=2)', function t() {
	var tc = upper_two_rhs;
	var AB = new Complex128Array( [
		0, 0, 4, 0,
		1, 1, 5, 0,
		2, -1, 6, 0
	] );
	var finfo = zpbtrf( 'upper', 3, 1, AB, 1, 2, 0 );
	assert.equal( finfo, 0 );
	// B has 2 RHS columns, LDB=3
	// b1 = A*[1,1+i,2], b2 = A*[i,0,1-i]
	var B = new Complex128Array( [
		4, 2, 10, 2, 13, 3,
		0, 4, 2, -2, 6, -6
	] );
	var info = zpbtrs( 'upper', 3, 1, 2, AB, 1, 2, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.B, 1e-13, 'B' );
});

test( 'zpbtrs: lower_two_rhs (UPLO=L, N=3, KD=1, NRHS=2)', function t() {
	var tc = lower_two_rhs;
	var AB = new Complex128Array( [
		4, 0, 1, -1,
		5, 0, 2, 1,
		6, 0, 0, 0
	] );
	var finfo = zpbtrf( 'lower', 3, 1, AB, 1, 2, 0 );
	assert.equal( finfo, 0 );
	var B = new Complex128Array( [
		4, 2, 10, 2, 13, 3,
		0, 4, 2, -2, 6, -6
	] );
	var info = zpbtrs( 'lower', 3, 1, 2, AB, 1, 2, 0, B, 1, 3, 0 );
	assert.equal( info, tc.info );
	assertArrayClose( Array.from( reinterpret( B, 0 ) ), tc.B, 1e-13, 'B' );
});
