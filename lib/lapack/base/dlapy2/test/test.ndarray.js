/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlapy2 = require( './../lib/ndarray.js' );

// FIXTURES //

var basic = require( './fixtures/basic.json' );
var x_zero = require( './fixtures/x_zero.json' );
var y_zero = require( './fixtures/y_zero.json' );
var both_zero = require( './fixtures/both_zero.json' );
var large = require( './fixtures/large.json' );
var negative = require( './fixtures/negative.json' );

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

test( 'dlapy2: basic 3,4 -> 5', function t() {
	var tc = basic;
	assertClose( dlapy2( 3.0, 4.0 ), tc.result, 1e-14, 'result' );
});

test( 'dlapy2: x=0', function t() {
	var tc = x_zero;
	assertClose( dlapy2( 0.0, 5.0 ), tc.result, 1e-14, 'result' );
});

test( 'dlapy2: y=0', function t() {
	var tc = y_zero;
	assertClose( dlapy2( 7.0, 0.0 ), tc.result, 1e-14, 'result' );
});

test( 'dlapy2: both zero', function t() {
	var tc = both_zero;
	assert.strictEqual( dlapy2( 0.0, 0.0 ), tc.result );
});

test( 'dlapy2: large values', function t() {
	var tc = large;
	assertClose( dlapy2( 1e+154, 1e+154 ), tc.result, 1e-14, 'result' );
});

test( 'dlapy2: negative values', function t() {
	var tc = negative;
	assertClose( dlapy2( -3.0, -4.0 ), tc.result, 1e-14, 'result' );
});

test( 'dlapy2: NaN propagation (x=NaN)', function t() {
	var result = dlapy2( NaN, 3.0 );
	assert.ok( result !== result, 'expected NaN' );
});

test( 'dlapy2: NaN propagation (y=NaN)', function t() {
	var result = dlapy2( 3.0, NaN );
	assert.ok( result !== result, 'expected NaN' );
});
