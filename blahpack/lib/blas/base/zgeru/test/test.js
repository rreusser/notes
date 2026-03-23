

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zgeru = require( './../lib' );

test( 'zgeru: main export is a function', function t() {
	assert.strictEqual( typeof zgeru, 'function' );
});

test( 'zgeru: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgeru.ndarray, 'function' );
});

// TODO: Add implementation tests
