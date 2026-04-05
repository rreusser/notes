/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlapll = require( './../lib/zlapll.js' );


// TESTS //

test( 'zlapll is a function', function t() {
	assert.strictEqual( typeof zlapll, 'function', 'is a function' );
});

test( 'zlapll has expected arity', function t() {
	assert.strictEqual( zlapll.length, 6, 'has expected arity' );
});

test( 'zlapll throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zlapll( -1, 2, 1, 2, 1, 2 );
	}, RangeError );
});
