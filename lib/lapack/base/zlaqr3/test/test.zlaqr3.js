/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqr3 = require( './../lib/zlaqr3.js' );


// TESTS //

test( 'zlaqr3 is a function', function t() {
	assert.strictEqual( typeof zlaqr3, 'function', 'is a function' );
});

test( 'zlaqr3 has expected arity', function t() {
	assert.strictEqual( zlaqr3.length, 2, 'has expected arity' );
});
