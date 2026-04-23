
/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zunm22 = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zunm22, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zunm22.ndarray, 'function', 'has ndarray method' );
});

test( 'main export arity', function t() {
	assert.strictEqual( zunm22.length, 14, 'has expected arity' );
	assert.strictEqual( zunm22.ndarray.length, 18, 'ndarray has expected arity' );
});
