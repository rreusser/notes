/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';


// MODULES //

var test = require( 'node:test' );
var readFileSync = require( 'fs' ).readFileSync;
var path = require( 'path' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsyr2k = require( './../lib/base.js' );
var ndarray = require( './../lib/ndarray.js' );


// FIXTURES //

var fixtureDir = path.join( __dirname, '..', '..', '..', '..', '..', 'test', 'fixtures' ); // eslint-disable-line max-len
var lines = readFileSync( path.join( fixtureDir, 'dsyr2k.jsonl' ), 'utf8' ).trim().split( '\n' ); // eslint-disable-line node/no-sync
var fixture = lines.map( function parse( line ) {
	return JSON.parse( line );
} );


// FUNCTIONS //

/**
* Returns a test case from the fixture data.
*
* @private
* @param {string} name - test case name
* @returns {*} result
*/
function findCase( name ) {
	return fixture.find( function find( t ) { return t.name === name;
	} );
}

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

test( 'dsyr2k: upper_N', function t() {
	var tc = findCase( 'upper_N' );

	// A is 3x2 col-major, B is 3x2 col-major, C is 3x3
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 2, 2.0, A, 1, 3, 0, B, 1, 3, 0, 1.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: lower_N', function t() {
	var tc = findCase( 'lower_N' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dsyr2k( 'lower', 'no-transpose', 3, 2, 2.0, A, 1, 3, 0, B, 1, 3, 0, 1.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: upper_T', function t() {
	var tc = findCase( 'upper_T' );

	// A is 2x3 col-major (K=2, N=3), B is 2x3 col-major
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dsyr2k( 'upper', 'transpose', 3, 2, 2.0, A, 1, 2, 0, B, 1, 2, 0, 1.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: lower_T', function t() {
	var tc = findCase( 'lower_T' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dsyr2k( 'lower', 'transpose', 3, 2, 2.0, A, 1, 2, 0, B, 1, 2, 0, 1.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: alpha_zero', function t() {
	var tc = findCase( 'alpha_zero' );
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( [ 2, 0, 0, 3, 4, 0, 5, 6, 7 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, B, 1, 3, 0, 2.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: beta_zero', function t() {
	var tc = findCase( 'beta_zero' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 99, 0, 0, 0, 99, 0, 0, 0, 99 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: n_zero', function t() {
	var result;
	var A;
	var B;
	var C;

	A = new Float64Array( 1 );
	B = new Float64Array( 1 );
	C = new Float64Array( 1 );
	result = dsyr2k( 'upper', 'no-transpose', 0, 2, 1.0, A, 1, 1, 0, B, 1, 1, 0, 1.0, C, 1, 1, 0 ); // eslint-disable-line max-len
	assert.ok( result === C );
});

test( 'dsyr2k: alpha_zero_beta_zero', function t() {
	var tc = findCase( 'alpha_zero_beta_zero' );
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( [ 5, 0, 0, 6, 7, 0, 8, 9, 10 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: alpha_zero_beta_zero_lower', function t() {
	var tc = findCase( 'alpha_zero_beta_zero_lower' );
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( [ 5, 6, 7, 0, 8, 9, 0, 0, 10 ] );
	dsyr2k( 'lower', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: alpha_zero_beta_scale_upper', function t() {
	var tc = findCase( 'alpha_zero_beta_scale_upper' );
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( [ 2, 0, 0, 3, 4, 0, 5, 6, 7 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, B, 1, 3, 0, 3.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: alpha_zero_beta_scale_lower', function t() {
	var tc = findCase( 'alpha_zero_beta_scale_lower' );
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( [ 2, 3, 5, 0, 4, 6, 0, 0, 7 ] );
	dsyr2k( 'lower', 'no-transpose', 3, 2, 0.0, A, 1, 3, 0, B, 1, 3, 0, 3.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: upper_N_beta_half', function t() {
	var tc = findCase( 'upper_N_beta_half' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, B, 1, 3, 0, 0.5, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: lower_N_beta_zero', function t() {
	var tc = findCase( 'lower_N_beta_zero' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 99, 0, 0, 0, 99, 0, 0, 0, 99 ] );
	dsyr2k( 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, B, 1, 3, 0, 0.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: lower_N_beta_half', function t() {
	var tc = findCase( 'lower_N_beta_half' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ] );
	dsyr2k( 'lower', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, B, 1, 3, 0, 0.5, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: k_zero_beta_scale', function t() {
	var tc = findCase( 'k_zero_beta_scale' );
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( [ 2, 0, 0, 3, 4, 0, 5, 6, 7 ] );
	dsyr2k( 'upper', 'no-transpose', 3, 0, 1.0, A, 1, 3, 0, B, 1, 3, 0, 2.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: upper_T_beta_zero', function t() {
	var tc = findCase( 'upper_T_beta_zero' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 99, 0, 0, 0, 99, 0, 0, 0, 99 ] );
	dsyr2k( 'upper', 'transpose', 3, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

test( 'dsyr2k: lower_T_beta_zero', function t() {
	var tc = findCase( 'lower_T_beta_zero' );
	var A = new Float64Array( [ 1, 2, 3, 4, 5, 6 ] );
	var B = new Float64Array( [ 0.5, 1.5, 2.5, 3.5, 4.5, 5.5 ] );
	var C = new Float64Array( [ 99, 0, 0, 0, 99, 0, 0, 0, 99 ] );
	dsyr2k( 'lower', 'transpose', 3, 2, 1.0, A, 1, 2, 0, B, 1, 2, 0, 0.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	assertArrayClose( toArray( C ), tc.c, 1e-14, 'c' );
});

// NDARRAY VALIDATION TESTS //

test( 'ndarray: throws TypeError for invalid uplo', function t() {
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( 9 );
	assert.throws( function f() {
		ndarray( 'invalid', 'no-transpose', 3, 2, 1.0, A, 1, 3, 0, B, 1, 3, 0, 1.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'ndarray: throws TypeError for invalid trans', function t() {
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( 9 );
	assert.throws( function f() {
		ndarray( 'upper', 'invalid', 3, 2, 1.0, A, 1, 3, 0, B, 1, 3, 0, 1.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	}, TypeError );
});

test( 'ndarray: throws RangeError for negative N', function t() {
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( 9 );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', -1, 2, 1.0, A, 1, 3, 0, B, 1, 3, 0, 1.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	}, RangeError );
});

test( 'ndarray: throws RangeError for negative K', function t() {
	var A = new Float64Array( 6 );
	var B = new Float64Array( 6 );
	var C = new Float64Array( 9 );
	assert.throws( function f() {
		ndarray( 'upper', 'no-transpose', 3, -1, 1.0, A, 1, 3, 0, B, 1, 3, 0, 1.0, C, 1, 3, 0 ); // eslint-disable-line max-len
	}, RangeError );
});
