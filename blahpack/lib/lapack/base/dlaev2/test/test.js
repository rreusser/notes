

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlaev2 = require( './../lib' );

test( 'dlaev2: main export is a function', function t() {
	assert.strictEqual( typeof dlaev2, 'function' );
});

test( 'dlaev2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlaev2.ndarray, 'function' );
});

// TODO: Add implementation tests
