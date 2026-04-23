/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zhetrsRook = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof zhetrsRook, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof zhetrsRook.ndarray, 'function', 'has ndarray method' );
});

test( 'main export and ndarray are distinct functions', function t() {
	assert.notStrictEqual( zhetrsRook, zhetrsRook.ndarray, 'are distinct' );
	assert.strictEqual( typeof zhetrsRook.ndarray, 'function', 'is a function' );
});
