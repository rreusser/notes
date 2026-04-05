/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqsb = require( './../lib/zlaqsb.js' );


// TESTS //

test( 'zlaqsb is a function', function t() {
	assert.strictEqual( typeof zlaqsb, 'function', 'is a function' );
});

test( 'zlaqsb has expected arity', function t() {
	assert.strictEqual( zlaqsb.length, 9, 'has expected arity' );
});

test( 'zlaqsb throws TypeError for invalid uplo', function t() {
	assert.throws( function throws() {
		zlaqsb( 'invalid', new Float64Array( 4 ), new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2 );
	}, TypeError );
});

test( 'zlaqsb throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlaqsb( 'upper', -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, new Float64Array( 4 ), 1, 2, 2 );
	}, RangeError );
});
