/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqge = require( './../lib/zlaqge.js' );


// TESTS //

test( 'zlaqge is a function', function t() {
	assert.strictEqual( typeof zlaqge, 'function', 'is a function' );
});

test( 'zlaqge has expected arity', function t() {
	assert.strictEqual( zlaqge.length, 11, 'has expected arity' );
});

test( 'zlaqge throws RangeError for negative M', function t() {
	assert.throws( function throws() {
		zlaqge( -1, new Float64Array( 4 ), new Float64Array( 4 ), 2, 2, 1, 2, 1, 2, 2, 2 );
	}, RangeError );
});

test( 'zlaqge throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlaqge( new Float64Array( 4 ), -1, new Float64Array( 4 ), 2, 2, 1, 2, 1, 2, 2, 2 );
	}, RangeError );
});
