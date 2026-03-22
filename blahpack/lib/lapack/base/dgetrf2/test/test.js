

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dgetrf2 = require( './../lib' );

test( 'dgetrf2: main export is a function', function t() {
	assert.strictEqual( typeof dgetrf2, 'function' );
});

test( 'dgetrf2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dgetrf2.ndarray, 'function' );
});

// TODO: Add implementation tests
