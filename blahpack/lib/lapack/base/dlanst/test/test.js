

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlanst = require( './../lib' );

test( 'dlanst: main export is a function', function t() {
	assert.strictEqual( typeof dlanst, 'function' );
});

test( 'dlanst: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlanst.ndarray, 'function' );
});

// TODO: Add implementation tests
