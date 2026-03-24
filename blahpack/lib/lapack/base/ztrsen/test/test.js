

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var ztrsen = require( './../lib' );

test( 'ztrsen: main export is a function', function t() {
	assert.strictEqual( typeof ztrsen, 'function' );
});

test( 'ztrsen: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof ztrsen.ndarray, 'function' );
});

// TODO: Add implementation tests
