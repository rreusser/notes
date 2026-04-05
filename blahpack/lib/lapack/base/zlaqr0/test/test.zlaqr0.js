/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqr0 = require( './../lib/zlaqr0.js' );


// TESTS //

test( 'zlaqr0 is a function', function t() {
	assert.strictEqual( typeof zlaqr0, 'function', 'is a function' );
});

test( 'zlaqr0 has expected arity', function t() {
	assert.strictEqual( zlaqr0.length, 2, 'has expected arity' );
});
