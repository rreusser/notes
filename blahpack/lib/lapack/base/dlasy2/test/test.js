

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlasy2 = require( './../lib' );

test( 'dlasy2: main export is a function', function t() {
	assert.strictEqual( typeof dlasy2, 'function' );
});

test( 'dlasy2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlasy2.ndarray, 'function' );
});

// TODO: Add implementation tests
