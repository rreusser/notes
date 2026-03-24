

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zlahqr = require( './../lib' );

test( 'zlahqr: main export is a function', function t() {
	assert.strictEqual( typeof zlahqr, 'function' );
});

test( 'zlahqr: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlahqr.ndarray, 'function' );
});

// TODO: Add implementation tests
