

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlaqr3 = require( './../lib' );

test( 'dlaqr3: main export is a function', function t() {
	assert.strictEqual( typeof dlaqr3, 'function' );
});

test( 'dlaqr3: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlaqr3.ndarray, 'function' );
});

// TODO: Add implementation tests
