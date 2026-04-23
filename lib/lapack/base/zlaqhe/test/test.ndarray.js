

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var Float64Array = require( '@stdlib/array/float64' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zlaqhe = require( './../lib/base.js' );

// FIXTURES //

var upper_equil = require( './fixtures/upper_equil.json' );
var lower_equil = require( './fixtures/lower_equil.json' );
var no_equil = require( './fixtures/no_equil.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );

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

/**
* Creates the standard 3x3 Hermitian test matrix in column-major:
* A = [[4, 1+i, 0], [1-i, 9, 1], [0, 1, 16]]
*/
function makeA3() {
	return new Complex128Array( [ 4, 0, 1, -1, 0, 0, 1, 1, 9, 0, 1, 0, 0, 0, 1, 0, 16, 0 ] );
}

// TESTS //

test( 'zlaqhe: upper_equil', function t() {
	var tc = upper_equil;
	var A = makeA3();
	var s = new Float64Array( [ 0.5, 1.0/3.0, 0.25 ] );
	var equed = zlaqhe( 'upper', 3, A, 1, 3, 0, s, 1, 0, 0.05, 16.0 );
	assert.equal( equed, 'yes', 'equed' );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
});

test( 'zlaqhe: lower_equil', function t() {
	var tc = lower_equil;
	var A = makeA3();
	var s = new Float64Array( [ 0.5, 1.0/3.0, 0.25 ] );
	var equed = zlaqhe( 'lower', 3, A, 1, 3, 0, s, 1, 0, 0.05, 16.0 );
	assert.equal( equed, 'yes', 'equed' );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
});

test( 'zlaqhe: no_equil', function t() {
	var tc = no_equil;
	var A = makeA3();
	var equed = zlaqhe( 'upper', 3, A, 1, 3, 0, new Float64Array( [ 1, 1, 1 ] ), 1, 0, 1.0, 16.0 );
	assert.equal( equed, 'none', 'equed' );
	// A should be unchanged
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
});

test( 'zlaqhe: n_zero', function t() {
	var tc = n_zero;
	var A = new Complex128Array( 1 );
	var equed = zlaqhe( 'upper', 0, A, 1, 1, 0, new Float64Array( 1 ), 1, 0, 1.0, 1.0 );
	assert.equal( equed, 'none', 'equed' );
});

test( 'zlaqhe: n_one', function t() {
	var tc = n_one;
	var A = new Complex128Array( [ 4, 0 ] );
	var equed = zlaqhe( 'upper', 1, A, 1, 1, 0, new Float64Array( [ 0.5 ] ), 1, 0, 0.01, 4.0 );
	assert.equal( equed, 'yes', 'equed' );
	assertArrayClose( Array.from( reinterpret( A, 0 ) ), tc.a, 1e-14, 'a' );
});
