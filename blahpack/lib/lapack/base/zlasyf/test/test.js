

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zlasyf = require( './../lib' );

test( 'zlasyf: main export is a function', function t() {
	assert.strictEqual( typeof zlasyf, 'function' );
});

test( 'zlasyf: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zlasyf.ndarray, 'function' );
});

// TODO: Add implementation tests
