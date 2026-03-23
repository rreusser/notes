

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zsyr = require( './../lib' );

test( 'zsyr: main export is a function', function t() {
	assert.strictEqual( typeof zsyr, 'function' );
});

test( 'zsyr: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zsyr.ndarray, 'function' );
});

// TODO: Add implementation tests
