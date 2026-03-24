

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlaqr2 = require( './../lib' );

test( 'dlaqr2: main export is a function', function t() {
	assert.strictEqual( typeof dlaqr2, 'function' );
});

test( 'dlaqr2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlaqr2.ndarray, 'function' );
});

// TODO: Add implementation tests
