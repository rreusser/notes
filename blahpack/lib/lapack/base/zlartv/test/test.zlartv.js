/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlartv = require( './../lib/zlartv.js' );


// TESTS //

test( 'zlartv is a function', function t() {
	assert.strictEqual( typeof zlartv, 'function', 'is a function' );
});

test( 'zlartv has expected arity', function t() {
	assert.strictEqual( zlartv.length, 8, 'has expected arity' );
});

test( 'zlartv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlartv( -1, 2, 1, 2, 1, 2, 2, 1 );
	}, RangeError );
});
