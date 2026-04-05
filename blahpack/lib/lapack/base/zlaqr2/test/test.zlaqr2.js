/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var zlaqr2 = require( './../lib/zlaqr2.js' );


// TESTS //

test( 'zlaqr2 is a function', function t() {
	assert.strictEqual( typeof zlaqr2, 'function', 'is a function' );
});

test( 'zlaqr2 has expected arity', function t() {
	assert.strictEqual( zlaqr2.length, 2, 'has expected arity' );
});
