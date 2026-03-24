

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlarfx = require( './../lib' );

test( 'dlarfx: main export is a function', function t() {
	assert.strictEqual( typeof dlarfx, 'function' );
});

test( 'dlarfx: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlarfx.ndarray, 'function' );
});

// TODO: Add implementation tests
