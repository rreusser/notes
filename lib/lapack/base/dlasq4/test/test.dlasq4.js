/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlasq4 = require( './../lib/dlasq4.js' );


// TESTS //

test( 'dlasq4 is a function', function t() {
	assert.strictEqual( typeof dlasq4, 'function', 'is a function' );
});

test( 'dlasq4 has expected arity', function t() {
	assert.strictEqual( dlasq4.length, 15, 'has expected arity' );
});
