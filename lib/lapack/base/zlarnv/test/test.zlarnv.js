/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlarnv = require( './../lib/zlarnv.js' );


// TESTS //

test( 'zlarnv is a function', function t() {
	assert.strictEqual( typeof zlarnv, 'function', 'is a function' );
});

test( 'zlarnv has expected arity', function t() {
	assert.strictEqual( zlarnv.length, 6, 'has expected arity' );
});

test( 'zlarnv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlarnv( 2, 2, 1, -1, 2, 1 );
	}, RangeError );
});
