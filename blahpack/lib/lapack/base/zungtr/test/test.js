

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zungtr = require( './../lib' );

test( 'zungtr: main export is a function', function t() {
	assert.strictEqual( typeof zungtr, 'function' );
});

test( 'zungtr: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zungtr.ndarray, 'function' );
});

// TODO: Add implementation tests
