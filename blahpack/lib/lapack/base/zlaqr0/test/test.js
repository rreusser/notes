

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zlaqr0 = require( './../lib' );

test( 'zlaqr0: main export is a function', function t() {
	assert.strictEqual( typeof zlaqr0, 'function' );
});

test( 'zlaqr0: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlaqr0.ndarray, 'function' );
});

// TODO: Add implementation tests
