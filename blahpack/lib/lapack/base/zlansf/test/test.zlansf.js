/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlansf = require( './../lib/zlansf.js' );


// TESTS //

test( 'zlansf is a function', function t() {
	assert.strictEqual( typeof zlansf, 'function', 'is a function' );
});

test( 'zlansf throws TypeError for invalid transr', function t() {
	assert.throws( function throws() {
		zlansf( 'max', 'invalid', 'upper', 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zlansf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlansf( 'max', 'no-transpose', 'invalid', 2, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zlansf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlansf( 'max', 'no-transpose', 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
