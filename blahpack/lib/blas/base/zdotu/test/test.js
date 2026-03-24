

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zdotu = require( './../lib' );

test( 'zdotu: main export is a function', function t() {
	assert.strictEqual( typeof zdotu, 'function' );
});

test( 'zdotu: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zdotu.ndarray, 'function' );
});

// TODO: Add implementation tests
