/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zsyconRook = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zsyconRook, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zsyconRook.ndarray, 'function', 'has ndarray method' );
});

test( 'main export expected arity', function t() {
	assert.strictEqual( zsyconRook.length, 12, 'main has expected arity' );
	assert.strictEqual( zsyconRook.ndarray.length, 14, 'ndarray has expected arity' );
});
