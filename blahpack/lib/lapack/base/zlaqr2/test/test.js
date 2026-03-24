

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zlaqr2 = require( './../lib' );

test( 'zlaqr2: main export is a function', function t() {
	assert.strictEqual( typeof zlaqr2, 'function' );
});

test( 'zlaqr2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlaqr2.ndarray, 'function' );
});

// TODO: Add implementation tests
