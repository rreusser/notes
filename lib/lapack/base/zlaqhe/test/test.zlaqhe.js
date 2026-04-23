/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqhe = require( './../lib/zlaqhe.js' );


// TESTS //

test( 'zlaqhe is a function', function t() {
	assert.strictEqual( typeof zlaqhe, 'function', 'is a function' );
});

test( 'zlaqhe has expected arity', function t() {
	assert.strictEqual( zlaqhe.length, 8, 'has expected arity' );
});

test( 'zlaqhe throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlaqhe( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 2 );
	}, TypeError );
});

test( 'zlaqhe throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlaqhe( 'upper', -1, new Float64Array( 4 ), 2, 2, 1, 2, 2 );
	}, RangeError );
});
