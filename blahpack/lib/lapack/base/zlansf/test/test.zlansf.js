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

test( 'zlansf has expected arity', function t() {
	assert.strictEqual( zlansf.length, 6, 'has expected arity' );
});

test( 'zlansf throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlansf( 2, 2, 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ) );
	}, TypeError );
});

test( 'zlansf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlansf( 2, 2, 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ) );
	}, RangeError );
});
