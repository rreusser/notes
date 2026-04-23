/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zpttrf = require( './../lib/zpttrf.js' );


// TESTS //

test( 'zpttrf is a function', function t() {
	assert.strictEqual( typeof zpttrf, 'function', 'is a function' );
});

test( 'zpttrf has expected arity', function t() {
	assert.strictEqual( zpttrf.length, 5, 'has expected arity' );
});

test( 'zpttrf throws RangeError for negative N', function t() {
	assert.throws( function throws() {
		zpttrf( -1, 2, 1, 2, 1 );
	}, RangeError );
});
