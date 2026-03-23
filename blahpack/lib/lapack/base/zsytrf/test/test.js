

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zsytrf = require( './../lib' );

test( 'zsytrf: main export is a function', function t() {
	assert.strictEqual( typeof zsytrf, 'function' );
});

test( 'zsytrf: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zsytrf.ndarray, 'function' );
});

// TODO: Add implementation tests
