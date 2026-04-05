/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlantb = require( './../lib/zlantb.js' );


// TESTS //

test( 'zlantb is a function', function t() {
	assert.strictEqual( typeof zlantb, 'function', 'is a function' );
});

test( 'zlantb has expected arity', function t() {
	assert.strictEqual( zlantb.length, 9, 'has expected arity' );
});

test( 'zlantb throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlantb( 2, 'invalid', 'non-unit', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlantb throws TypeError for invalid diag', function t() {
	assert.throws( function throws() {
		zlantb( 2, 'upper', 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, TypeError );
});

test( 'zlantb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlantb( 2, 'upper', 'non-unit', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});

test( 'zlantb throws RangeError for negative K', function t() {
	assert.throws( function throws() {
		zlantb( 2, 'upper', 'non-unit', new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, new Float64Array( 4 ), 1 );
	}, RangeError );
});
