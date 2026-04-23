/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var Float64Array = require( '@stdlib/array/float64' );
var dlasq6 = require( './../lib/dlasq6.js' );


// TESTS //

test( 'dlasq6 is a function', function t() {
	assert.strictEqual( typeof dlasq6, 'function', 'is a function' );
});

test( 'dlasq6 has expected arity', function t() {
	assert.strictEqual( dlasq6.length, 5, 'has expected arity' );
});
