

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlahr2 = require( './../lib' );

test( 'dlahr2: main export is a function', function t() {
	assert.strictEqual( typeof dlahr2, 'function' );
});

test( 'dlahr2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlahr2.ndarray, 'function' );
});

// TODO: Add implementation tests
