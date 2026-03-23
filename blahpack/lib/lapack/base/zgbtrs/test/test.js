

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zgbtrs = require( './../lib' );

test( 'zgbtrs: main export is a function', function t() {
	assert.strictEqual( typeof zgbtrs, 'function' );
});

test( 'zgbtrs: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgbtrs.ndarray, 'function' );
});

// TODO: Add implementation tests
