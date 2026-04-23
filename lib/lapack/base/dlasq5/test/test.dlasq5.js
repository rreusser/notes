/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlasq5 = require( './../lib/dlasq5.js' );


// TESTS //

test( 'dlasq5 is a function', function t() {
	assert.strictEqual( typeof dlasq5, 'function', 'is a function' );
});

test( 'dlasq5 has expected arity', function t() {
	assert.strictEqual( dlasq5.length, 9, 'has expected arity' );
});
