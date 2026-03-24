

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlaln2 = require( './../lib' );

test( 'dlaln2: main export is a function', function t() {
	assert.strictEqual( typeof dlaln2, 'function' );
});

test( 'dlaln2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlaln2.ndarray, 'function' );
});

// TODO: Add implementation tests
