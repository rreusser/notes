

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var izmax1 = require( './../lib' );

test( 'izmax1: main export is a function', function t() {
	assert.strictEqual( typeof izmax1, 'function' );
});

test( 'izmax1: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof izmax1.ndarray, 'function' );
});

// TODO: Add implementation tests
