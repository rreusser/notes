

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zlatrs = require( './../lib' );

test( 'zlatrs: main export is a function', function t() {
	assert.strictEqual( typeof zlatrs, 'function' );
});

test( 'zlatrs: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlatrs.ndarray, 'function' );
});

// TODO: Add implementation tests
