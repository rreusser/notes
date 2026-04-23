/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dsptrd = require( './../lib/base.js' );

// FIXTURES //

var upper_4x4 = require( './fixtures/upper_4x4.json' );
var lower_4x4 = require( './fixtures/lower_4x4.json' );
var upper_3x3 = require( './fixtures/upper_3x3.json' );
var lower_3x3 = require( './fixtures/lower_3x3.json' );
var n_one_upper = require( './fixtures/n_one_upper.json' );
var n_one_lower = require( './fixtures/n_one_lower.json' );
var upper_diagonal = require( './fixtures/upper_diagonal.json' );
var lower_diagonal = require( './fixtures/lower_diagonal.json' );

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

// TESTS //

test( 'dsptrd: upper_4x4', function t() {
	var info;
	var tau;
	var tc;
	var AP;
	var d;
	var e;

	tc = upper_4x4;
	AP = new Float64Array( [ 4, 1, 5, 2, 1, 6, 1, 2, 1, 7 ] );
	d = new Float64Array( 4 );
	e = new Float64Array( 3 );
	tau = new Float64Array( 3 );
	info = dsptrd( 'upper', 4, AP, 1, 0, d, 1, 0, e, 1, 0, tau, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
	assertArrayClose( tau, tc.tau, 1e-14, 'tau' );
});

test( 'dsptrd: lower_4x4', function t() {
	var info;
	var tau;
	var tc;
	var AP;
	var d;
	var e;

	tc = lower_4x4;
	AP = new Float64Array( [ 4, 1, 2, 1, 5, 1, 2, 6, 1, 7 ] );
	d = new Float64Array( 4 );
	e = new Float64Array( 3 );
	tau = new Float64Array( 3 );
	info = dsptrd( 'lower', 4, AP, 1, 0, d, 1, 0, e, 1, 0, tau, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
	assertArrayClose( tau, tc.tau, 1e-14, 'tau' );
});

test( 'dsptrd: upper_3x3', function t() {
	var info;
	var tau;
	var tc;
	var AP;
	var d;
	var e;

	tc = upper_3x3;
	AP = new Float64Array( [ 2, 3, 5, 1, 4, 8 ] );
	d = new Float64Array( 3 );
	e = new Float64Array( 2 );
	tau = new Float64Array( 2 );
	info = dsptrd( 'upper', 3, AP, 1, 0, d, 1, 0, e, 1, 0, tau, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
	assertArrayClose( tau, tc.tau, 1e-14, 'tau' );
});

test( 'dsptrd: lower_3x3', function t() {
	var info;
	var tau;
	var tc;
	var AP;
	var d;
	var e;

	tc = lower_3x3;
	AP = new Float64Array( [ 2, 3, 1, 5, 4, 8 ] );
	d = new Float64Array( 3 );
	e = new Float64Array( 2 );
	tau = new Float64Array( 2 );
	info = dsptrd( 'lower', 3, AP, 1, 0, d, 1, 0, e, 1, 0, tau, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( AP, tc.AP, 1e-14, 'AP' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
	assertArrayClose( tau, tc.tau, 1e-14, 'tau' );
});

test( 'dsptrd: n_one_upper', function t() {
	var info;
	var tau;
	var tc;
	var AP;
	var d;
	var e;

	tc = n_one_upper;
	AP = new Float64Array( [ 3 ] );
	d = new Float64Array( 1 );
	e = new Float64Array( 0 );
	tau = new Float64Array( 0 );
	info = dsptrd( 'upper', 1, AP, 1, 0, d, 1, 0, e, 1, 0, tau, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( AP[ 0 ], tc.AP1, 1e-14, 'AP1' );
	assertClose( d[ 0 ], tc.d1, 1e-14, 'd1' );
});

test( 'dsptrd: n_one_lower', function t() {
	var info;
	var tau;
	var tc;
	var AP;
	var d;
	var e;

	tc = n_one_lower;
	AP = new Float64Array( [ 3 ] );
	d = new Float64Array( 1 );
	e = new Float64Array( 0 );
	tau = new Float64Array( 0 );
	info = dsptrd( 'lower', 1, AP, 1, 0, d, 1, 0, e, 1, 0, tau, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertClose( AP[ 0 ], tc.AP1, 1e-14, 'AP1' );
	assertClose( d[ 0 ], tc.d1, 1e-14, 'd1' );
});

test( 'dsptrd: n_zero', function t() {
	var info;
	var tau;
	var AP;
	var d;
	var e;

	AP = new Float64Array( 0 );
	d = new Float64Array( 0 );
	e = new Float64Array( 0 );
	tau = new Float64Array( 0 );
	info = dsptrd( 'upper', 0, AP, 1, 0, d, 1, 0, e, 1, 0, tau, 1, 0 );
	assert.equal( info, 0, 'info' );
});

test( 'dsptrd: upper_diagonal', function t() {
	var info;
	var tau;
	var tc;
	var AP;
	var d;
	var e;

	tc = upper_diagonal;
	AP = new Float64Array( [ 2, 0, 5, 0, 0, 8 ] );
	d = new Float64Array( 3 );
	e = new Float64Array( 2 );
	tau = new Float64Array( 2 );
	info = dsptrd( 'upper', 3, AP, 1, 0, d, 1, 0, e, 1, 0, tau, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
	assertArrayClose( tau, tc.tau, 1e-14, 'tau' );
});

test( 'dsptrd: lower_diagonal', function t() {
	var info;
	var tau;
	var tc;
	var AP;
	var d;
	var e;

	tc = lower_diagonal;
	AP = new Float64Array( [ 2, 0, 0, 5, 0, 8 ] );
	d = new Float64Array( 3 );
	e = new Float64Array( 2 );
	tau = new Float64Array( 2 );
	info = dsptrd( 'lower', 3, AP, 1, 0, d, 1, 0, e, 1, 0, tau, 1, 0 );
	assert.equal( info, tc.info, 'info' );
	assertArrayClose( d, tc.d, 1e-14, 'd' );
	assertArrayClose( e, tc.e, 1e-14, 'e' );
	assertArrayClose( tau, tc.tau, 1e-14, 'tau' );
});
