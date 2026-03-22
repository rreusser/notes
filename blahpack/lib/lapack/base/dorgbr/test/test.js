

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dorgbr = require( './../lib' );

test( 'dorgbr: main export is a function', function t() {
	assert.strictEqual( typeof dorgbr, 'function' );
});

test( 'dorgbr: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dorgbr.ndarray, 'function' );
});

// TODO: Add implementation tests
