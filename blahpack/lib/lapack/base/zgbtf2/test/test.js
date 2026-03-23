

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zgbtf2 = require( './../lib' );

test( 'zgbtf2: main export is a function', function t() {
	assert.strictEqual( typeof zgbtf2, 'function' );
});

test( 'zgbtf2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgbtf2.ndarray, 'function' );
});

// TODO: Add implementation tests
