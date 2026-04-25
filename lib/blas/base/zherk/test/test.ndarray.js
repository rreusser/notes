/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var reinterpret = require( '@stdlib/strided/base/reinterpret-complex128' );
var zherk = require( './../lib/ndarray.js' );
var ndarray = require( './../lib/ndarray.js' );

// FIXTURES //

var upper_n = require( './fixtures/upper_n.json' );
var lower_n = require( './fixtures/lower_n.json' );
var upper_c = require( './fixtures/upper_c.json' );
var lower_c = require( './fixtures/lower_c.json' );
var alpha_zero = require( './fixtures/alpha_zero.json' );
var alpha_zero_beta_zero = require( './fixtures/alpha_zero_beta_zero.json' );
var alpha_zero_beta_zero_lower = require( './fixtures/alpha_zero_beta_zero_lower.json' );
var alpha_zero_beta_scale_lower = require( './fixtures/alpha_zero_beta_scale_lower.json' );
var beta_zero = require( './fixtures/beta_zero.json' );
var upper_n_beta_half = require( './fixtures/upper_n_beta_half.json' );
var lower_n_beta_zero = require( './fixtures/lower_n_beta_zero.json' );
var lower_n_beta_half = require( './fixtures/lower_n_beta_half.json' );
var upper_c_beta_zero = require( './fixtures/upper_c_beta_zero.json' );
var lower_c_beta_zero = require( './fixtures/lower_c_beta_zero.json' );

// FUNCTIONS //

/**
* Asserts that two numbers are approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertClose( actual, expected, tol, msg ) {
	var relErr = Math.abs( actual - expected ) / Math.max( Math.abs( expected ), 1.0 ); // eslint-disable-line max-len
	assert.ok( relErr <= tol, msg + ': expected ' + expected + ', got ' + actual );
}

/**
* Asserts that two arrays are element-wise approximately equal.
*
* @private
* @param {*} actual - actual value
* @param {*} expected - expected value
* @param {number} tol - tolerance
* @param {string} msg - assertion message
*/
function assertArrayClose( actual, expected, tol, msg ) {
	var i;
	assert.equal( actual.length, expected.length, msg + ': length mismatch' );
	for ( i = 0; i < expected.length; i++ ) {
		assertClose( actual[ i ], expected[ i ], tol, msg + '[' + i + ']' );
	}
}

/**
* Converts a typed array to a plain array.
*
* @private
* @param {TypedArray} arr - input array
* @returns {Array} output array
*/
function toArray( arr ) {
	var out = [];
	var i;
	for ( i = 0; i < arr.length; i++ ) {
		out.push( arr[ i ] );
	}
	return out;
}

// TESTS //

test( 'zherk: upper_N', function t() {
	var tc = upper_n;

	// A is 3x2 complex, C is 3x3 complex
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] ); // eslint-disable-line max-len
	zherk( 'upper', 'no-transpose', 3, 2, 2.0, A, 1, 3, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: lower_N', function t() {
	var tc = lower_n;
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] ); // eslint-disable-line max-len
	zherk( 'lower', 'no-transpose', 3, 2, 2.0, A, 1, 3, 0, 1.0, C, 1, 3, 0 );
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: upper_C', function t() {
	var tc = upper_c;

	// A is 2x3 complex
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] ); // eslint-disable-line max-len
	zherk( 'upper', 'conjugate-transpose', 3, 2, 2.0, A, 1, 2, 0, 1.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: lower_C', function t() {
	var tc = lower_c;
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] ); // eslint-disable-line max-len
	zherk( 'lower', 'conjugate-transpose', 3, 2, 2.0, A, 1, 2, 0, 1.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: alpha_zero', function t() {
	var tc = alpha_zero;
	var A = new Complex128Array( 6 );
	var C = new Complex128Array( [ 2, 0, 0, 0, 0, 0, 3, 1, 4, 0, 0, 0, 5, 2, 6, 3, 7, 0 ] ); // eslint-disable-line max-len
	zherk( 'upper', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, 2.0, C, 1, 3, 0 );
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: alpha_zero_beta_zero', function t() {
	var tc = alpha_zero_beta_zero;
	var A = new Complex128Array( 6 );
	var C = new Complex128Array( [ 5, 0, 0, 0, 0, 0, 6, 1, 7, 0, 0, 0, 8, 2, 9, 3, 10, 0 ] ); // eslint-disable-line max-len
	zherk( 'upper', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: alpha_zero_beta_zero_lower', function t() {
	var tc = alpha_zero_beta_zero_lower;
	var A = new Complex128Array( 6 );
	var C = new Complex128Array( [ 5, 0, 6, 1, 7, 2, 0, 0, 8, 0, 9, 3, 0, 0, 0, 0, 10, 0 ] ); // eslint-disable-line max-len
	zherk( 'lower', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: alpha_zero_beta_scale_lower', function t() {
	var tc = alpha_zero_beta_scale_lower;
	var A = new Complex128Array( 6 );
	var C = new Complex128Array( [ 2, 0, 3, 1, 5, 2, 0, 0, 4, 0, 6, 3, 0, 0, 0, 0, 7, 0 ] ); // eslint-disable-line max-len
	zherk( 'lower', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, 3.0, C, 1, 3, 0 );
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: beta_zero', function t() {
	var tc = beta_zero;
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 99, 0, 0, 0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 99, 0 ] ); // eslint-disable-line max-len
	zherk( 'upper', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: upper_N_beta_half', function t() {
	var tc = upper_n_beta_half;
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] ); // eslint-disable-line max-len
	zherk( 'upper', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 0.5, C, 1, 3, 0 );
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: lower_N_beta_zero', function t() {
	var tc = lower_n_beta_zero;
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 99, 0, 0, 0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 99, 0 ] ); // eslint-disable-line max-len
	zherk( 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 0.0, C, 1, 3, 0 );
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: lower_N_beta_half', function t() {
	var tc = lower_n_beta_half;
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0 ] ); // eslint-disable-line max-len
	zherk( 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, 0.5, C, 1, 3, 0 );
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: upper_C_beta_zero', function t() {
	var tc = upper_c_beta_zero;
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 99, 0, 0, 0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 99, 0 ] ); // eslint-disable-line max-len
	zherk( 'upper', 'conjugate-transpose', 3, 2, 1.0, A, 1, 2, 0, 0.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: lower_C_beta_zero', function t() {
	var tc = lower_c_beta_zero;
	var A = new Complex128Array( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] );
	var C = new Complex128Array( [ 99, 0, 0, 0, 0, 0, 0, 0, 99, 0, 0, 0, 0, 0, 0, 0, 99, 0 ] ); // eslint-disable-line max-len
	zherk( 'lower', 'conjugate-transpose', 3, 2, 1.0, A, 1, 2, 0, 0.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( reinterpret( C, 0 ) ), tc.c, 1e-14, 'c' );
});

test( 'zherk: n_zero quick return', function t() {
	var result;
	var A;
	var C;

	A = new Complex128Array( 1 );
	C = new Complex128Array( 1 );
	result = zherk( 'upper', 'no-transpose', 0, 2, 1.0, A, 1, 1, 0, 1.0, C, 1, 1, 0 ); // eslint-disable-line max-len
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
