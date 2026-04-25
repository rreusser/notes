

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var zpoequ = require( './../lib/ndarray.js' );

// FIXTURES //

var basic = require( './fixtures/basic.json' );
var diagonal_varied = require( './fixtures/diagonal_varied.json' );
var non_positive_diag = require( './fixtures/non_positive_diag.json' );
var zero_diag = require( './fixtures/zero_diag.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var identity = require( './fixtures/identity.json' );

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

test( 'zpoequ: basic', function t() {
	var tc = basic;
	// A = [[4, 1+i, 0], [1-i, 9, 1], [0, 1, 16]]
	var A = new Complex128Array( [ 4, 0, 1, -1, 0, 0, 1, 1, 9, 0, 1, 0, 0, 0, 1, 0, 16, 0 ] );
	var s = new Float64Array( 3 );
	var result = zpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'zpoequ: diagonal_varied', function t() {
	var tc = diagonal_varied;
	// A = diag(100, 1, 0.25)
	var A = new Complex128Array( [ 100, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0.25, 0 ] );
	var s = new Float64Array( 3 );
	var result = zpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'zpoequ: non_positive_diag', function t() {
	var tc = non_positive_diag;
	// A has diag [4, -1, 9]
	var A = new Complex128Array( [ 4, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 9, 0 ] );
	var s = new Float64Array( 3 );
	var result = zpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'zpoequ: zero_diag', function t() {
	var tc = zero_diag;
	// A has diag [4, 0, 9]
	var A = new Complex128Array( [ 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0 ] );
	var s = new Float64Array( 3 );
	var result = zpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
});

test( 'zpoequ: n_zero', function t() {
	var tc = n_zero;
	var A = new Complex128Array( 1 );
	var s = new Float64Array( 1 );
	var result = zpoequ( 0, A, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
});

test( 'zpoequ: n_one', function t() {
	var tc = n_one;
	var A = new Complex128Array( [ 25, 0 ] );
	var s = new Float64Array( 1 );
	var result = zpoequ( 1, A, 1, 1, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});

test( 'zpoequ: identity', function t() {
	var tc = identity;
	var A = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] );
	var s = new Float64Array( 3 );
	var result = zpoequ( 3, A, 1, 3, 0, s, 1, 0 );
	assert.equal( result.info, tc.info, 'info' );
	assertClose( result.scond, tc.scond, 1e-14, 'scond' );
	assertClose( result.amax, tc.amax, 1e-14, 'amax' );
	assertArrayClose( Array.from( s ), tc.s, 1e-14, 's' );
});
