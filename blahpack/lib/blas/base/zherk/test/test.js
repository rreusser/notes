'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zherk = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' );
var lines = readFileSync( path.join( fixtureDir, 'zherk.jsonl' ), 'utf8' ).trim().split( '\n' );
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

test( 'zherk: upper_N', function t() {
	var tc = findCase( 'upper_N' );
	// A is 3x2 complex, C is 3x3 complex
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] );
	zherk( 'upper', 'no-transpose', 3, 2, 2.0, A, 1, 3, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: lower_N', function t() {
	var tc = findCase( 'lower_N' );
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] );
	zherk( 'lower', 'no-transpose', 3, 2, 2.0, A, 1, 3, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: upper_C', function t() {
	var tc = findCase( 'upper_C' );
	// A is 2x3 complex
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] );
	zherk( 'upper', 'conjugate-transpose', 3, 2, 2.0, A, 1, 2, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: lower_C', function t() {
	var tc = findCase( 'lower_C' );
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] );
	zherk( 'lower', 'conjugate-transpose', 3, 2, 2.0, A, 1, 2, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: alpha_zero', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = new Complex128Array( 6 );
	var C = new Complex128Array( [ 2, 0, 0, 0, 0, 0, 3, 1, 4, 0, 0, 0, 5, 2, 6, 3, 7, 0 ] );
	zherk( 'upper', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, 2.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: alpha_zero_beta_zero', function t() {
	var tc = findCase( 'alpha_zero_beta_zero' );
	var A = new Complex128Array( 6 );
	var C = new Complex128Array( [ 5, 0, 0, 0, 0, 0, 6, 1, 7, 0, 0, 0, 8, 2, 9, 3, 10, 0 ] );
	zherk( 'upper', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: alpha_zero_beta_zero_lower', function t() {
	var tc = findCase( 'alpha_zero_beta_zero_lower' );
	var A = new Complex128Array( 6 );
	var C = new Complex128Array( [ 5, 0, 6, 1, 7, 2, 0, 0, 8, 0, 9, 3, 0, 0, 0, 0, 10, 0 ] );
	zherk( 'lower', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: alpha_zero_beta_scale_lower', function t() {
	var tc = findCase( 'alpha_zero_beta_scale_lower' );
	var A = new Complex128Array( 6 );
	var C = new Complex128Array( [ 2, 0, 3, 1, 5, 2, 0, 0, 4, 0, 6, 3, 0, 0, 0, 0, 7, 0 ] );
	zherk( 'lower', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, 3.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: beta_zero', function t() {
	var tc = findCase( 'beta_zero' );
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 99, 0, 0, 0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 99, 0 ] );
	zherk( 'upper', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: upper_N_beta_half', function t() {
	var tc = findCase( 'upper_N_beta_half' );
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] );
	zherk( 'upper', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 0.5, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: lower_N_beta_zero', function t() {
	var tc = findCase( 'lower_N_beta_zero' );
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 99, 0, 0, 0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 99, 0 ] );
	zherk( 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: lower_N_beta_half', function t() {
	var tc = findCase( 'lower_N_beta_half' );
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] );
	zherk( 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 0.5, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: upper_C_beta_zero', function t() {
	var tc = findCase( 'upper_C_beta_zero' );
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 99, 0, 0, 0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 99, 0 ] );
	zherk( 'upper', 'conjugate-transpose', 3, 2, 1.0, A, 1, 2, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: lower_C_beta_zero', function t() {
	var tc = findCase( 'lower_C_beta_zero' );
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 99, 0, 0, 0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 99, 0 ] );
	zherk( 'lower', 'conjugate-transpose', 3, 2, 1.0, A, 1, 2, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( Array.from( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: n_zero quick return', function t() {
	var A = new Complex128Array( 1 );
	var C = new Complex128Array( 1 );
	var result = zherk( 'upper', 'no-transpose', 0, 2, 1.0, A, 1, 1, 0, 1.0, C, 1, 1, 0 );
	assert.ok( result === C );
});

// NDARRAY VALIDATION TESTS //

test( 'ndarray: throws TypeError for invalid uplo', function t() {
	var A = new Complex128Array( 6 );
	var C = new Complex128Array( 9 );
	assert.throws( function f() {
		ndarray( 'invalid', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 1.0, C, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid trans', function t() {
	var A = new Complex128Array( 6 );
	var C = new Complex128Array( 9 );
	assert.throws( function f() {
		ndarray( 'upper', 'invalid', 3, 2, 1.0, A, 1, 3, 0, 1.0, C, 1, 3, 0 );
	}, TypeError );
});

test( 'ndarray: throws RangeError for negative N', function t() {
	var A = new Complex128Array( 6 );
	var C = new Complex128Array( 9 );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', -1, 2, 1.0, A, 1, 3, 0, 1.0, C, 1, 3, 0 );
	}, RangeError );
});

test( 'ndarray: throws RangeError for negative K', function t() {
	var A = new Complex128Array( 6 );
	var C = new Complex128Array( 9 );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 3, -1, 1.0, A, 1, 3, 0, 1.0, C, 1, 3, 0 );
	}, RangeError );
});
