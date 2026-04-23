/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqr1 = require( './../lib/zlaqr1.js' );


// TESTS //

test( 'zlaqr1 is a function', function t() {
	assert.strictEqual( typeof zlaqr1, 'function', 'is a function' );
});

test( 'zlaqr1 has expected arity', function t() {
	assert.strictEqual( zlaqr1.length, 2, 'has expected arity' );
});
