

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zlaqr5 = require( './../lib' );

test( 'zlaqr5: main export is a function', function t() {
	assert.strictEqual( typeof zlaqr5, 'function' );
});

test( 'zlaqr5: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlaqr5.ndarray, 'function' );
});

// TODO: Add implementation tests
