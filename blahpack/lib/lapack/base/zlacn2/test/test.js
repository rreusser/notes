

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zlacn2 = require( './../lib' );

test( 'zlacn2: main export is a function', function t() {
	assert.strictEqual( typeof zlacn2, 'function' );
});

test( 'zlacn2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlacn2.ndarray, 'function' );
});

// TODO: Add implementation tests
