

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zlaqr3 = require( './../lib' );

test( 'zlaqr3: main export is a function', function t() {
	assert.strictEqual( typeof zlaqr3, 'function' );
});

test( 'zlaqr3: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlaqr3.ndarray, 'function' );
});

// TODO: Add implementation tests
