

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlaqr4 = require( './../lib' );

test( 'dlaqr4: main export is a function', function t() {
	assert.strictEqual( typeof dlaqr4, 'function' );
});

test( 'dlaqr4: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlaqr4.ndarray, 'function' );
});

// TODO: Add implementation tests
