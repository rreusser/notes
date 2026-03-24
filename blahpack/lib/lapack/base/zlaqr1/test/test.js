

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zlaqr1 = require( './../lib' );

test( 'zlaqr1: main export is a function', function t() {
	assert.strictEqual( typeof zlaqr1, 'function' );
});

test( 'zlaqr1: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlaqr1.ndarray, 'function' );
});

// TODO: Add implementation tests
