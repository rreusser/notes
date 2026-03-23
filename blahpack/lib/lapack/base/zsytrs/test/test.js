

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zsytrs = require( './../lib' );

test( 'zsytrs: main export is a function', function t() {
	assert.strictEqual( typeof zsytrs, 'function' );
});

test( 'zsytrs: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zsytrs.ndarray, 'function' );
});

// TODO: Add implementation tests
