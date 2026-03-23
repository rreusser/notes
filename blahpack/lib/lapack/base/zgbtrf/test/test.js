

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zgbtrf = require( './../lib' );

test( 'zgbtrf: main export is a function', function t() {
	assert.strictEqual( typeof zgbtrf, 'function' );
});

test( 'zgbtrf: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgbtrf.ndarray, 'function' );
});

// TODO: Add implementation tests
