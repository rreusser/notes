

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zgees = require( './../lib' );

test( 'zgees: main export is a function', function t() {
	assert.strictEqual( typeof zgees, 'function' );
});

test( 'zgees: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgees.ndarray, 'function' );
});

// TODO: Add implementation tests
