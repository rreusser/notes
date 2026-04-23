/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zgttrf = require( './../lib/zgttrf.js' );


// TESTS //

test( 'zgttrf is a function', function t() {
	assert.strictEqual( typeof zgttrf, 'function', 'is a function' );
});

test( 'zgttrf has expected arity', function t() {
	assert.strictEqual( zgttrf.length, 2, 'has expected arity' );
});
