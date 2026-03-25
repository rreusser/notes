

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zgttrf = require( './../lib' );

test( 'zgttrf: main export is a function', function t() {
	assert.strictEqual( typeof zgttrf, 'function' );
});

test( 'zgttrf: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgttrf.ndarray, 'function' );
});

// TODO: Add implementation tests
