/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dorm22 = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dorm22, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dorm22.ndarray, 'function', 'has ndarray method' );
});

test( 'main export arity', function t() {
	assert.strictEqual( dorm22.length, 14, 'has expected arity' );
	assert.strictEqual( dorm22.ndarray.length, 18, 'ndarray has expected arity' );
});
