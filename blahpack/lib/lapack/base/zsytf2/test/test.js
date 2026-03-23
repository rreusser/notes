

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zsytf2 = require( './../lib' );

test( 'zsytf2: main export is a function', function t() {
	assert.strictEqual( typeof zsytf2, 'function' );
});

test( 'zsytf2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zsytf2.ndarray, 'function' );
});

// TODO: Add implementation tests
