

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zunglq = require( './../lib' );

test( 'zunglq: main export is a function', function t() {
	assert.strictEqual( typeof zunglq, 'function' );
});

test( 'zunglq: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zunglq.ndarray, 'function' );
});

// TODO: Add implementation tests
