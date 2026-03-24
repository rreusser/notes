

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dgeev = require( './../lib' );

test( 'dgeev: main export is a function', function t() {
	assert.strictEqual( typeof dgeev, 'function' );
});

test( 'dgeev: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dgeev.ndarray, 'function' );
});

// TODO: Add implementation tests
