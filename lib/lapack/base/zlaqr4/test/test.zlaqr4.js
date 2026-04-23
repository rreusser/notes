/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqr4 = require( './../lib/zlaqr4.js' );


// TESTS //

test( 'zlaqr4 is a function', function t() {
	assert.strictEqual( typeof zlaqr4, 'function', 'is a function' );
});

test( 'zlaqr4 has expected arity', function t() {
	assert.strictEqual( zlaqr4.length, 2, 'has expected arity' );
});
