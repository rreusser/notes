/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Complex128Array = require( '@stdlib/array/complex128' );
var dzasum = require( './../lib/ndarray.js' );

// FIXTURES //

var basic = require( './fixtures/basic.json' );
var n_zero = require( './fixtures/n_zero.json' );
var n_one = require( './fixtures/n_one.json' );
var stride2 = require( './fixtures/stride2.json' );

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

// TESTS //

test( 'dzasum: basic', function t() {
	var result;
	var tc;
	var zx;

	tc = basic;
	zx = new Complex128Array( [ 1.0, 2.0, 3.0, -4.0, -5.0, 6.0 ] );
	result = dzasum( 3, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzasum: n_zero', function t() {
	var result;
	var tc;
	var zx;

	tc = n_zero;
	zx = new Complex128Array( [ 1.0, 2.0, 3.0, -4.0, -5.0, 6.0 ] );
	result = dzasum( 0, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzasum: n_one', function t() {
	var result;
	var tc;
	var zx;

	tc = n_one;
	zx = new Complex128Array( [ 3.0, 4.0 ] );
	result = dzasum( 1, zx, 1, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});

test( 'dzasum: stride2', function t() {
	var result;
	var tc;
	var zx;

	tc = stride2;
	zx = new Complex128Array( [ 1.0, 1.0, 99.0, 99.0, 2.0, 3.0 ] );
	result = dzasum( 2, zx, 2, 0 );
	assertClose( result, tc.result, 1e-14, 'result' );
});
