/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlacgv = require( './../lib/zlacgv.js' );


// TESTS //

test( 'zlacgv is a function', function t() {
	assert.strictEqual( typeof zlacgv, 'function', 'is a function' );
});

test( 'zlacgv has expected arity', function t() {
	assert.strictEqual( zlacgv.length, 3, 'has expected arity' );
});

test( 'zlacgv throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlacgv( -1, 2, 1 );
	}, RangeError );
});
