/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlarfy = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dlarfy, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dlarfy.ndarray, 'function', 'has ndarray method' );
});

test( 'main and ndarray have the expected arity', function t() {
	assert.strictEqual( dlarfy.length, 10, 'main has arity 10' );
	assert.strictEqual( dlarfy.ndarray.length, 13, 'ndarray has arity 13' );
});
