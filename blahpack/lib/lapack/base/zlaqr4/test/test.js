

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zlaqr4 = require( './../lib' );

test( 'zlaqr4: main export is a function', function t() {
	assert.strictEqual( typeof zlaqr4, 'function' );
});

test( 'zlaqr4: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlaqr4.ndarray, 'function' );
});

// TODO: Add implementation tests
