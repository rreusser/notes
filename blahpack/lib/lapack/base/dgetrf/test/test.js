

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dgetrf = require( './../lib' );

test( 'dgetrf: main export is a function', function t() {
	assert.strictEqual( typeof dgetrf, 'function' );
});

test( 'dgetrf: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dgetrf.ndarray, 'function' );
});

// TODO: Add implementation tests
