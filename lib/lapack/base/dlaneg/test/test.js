/* eslint-disable no-restricted-syntax, stdlib/first-unit-test */

'use strict';

// MODULES //

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlaneg = require( './../lib' );


// TESTS //

test( 'main export is a function', function t() {
	assert.strictEqual( typeof dlaneg, 'function', 'main export is a function' );
});

test( 'main export has an ndarray method', function t() {
	assert.strictEqual( typeof dlaneg.ndarray, 'function', 'has ndarray method' );
});

test( 'main export and ndarray method have the expected arities', function t() {
	assert.strictEqual( dlaneg.length, 8, 'main has arity 8' );
	assert.strictEqual( dlaneg.ndarray.length, 10, 'ndarray has arity 10' );
});
