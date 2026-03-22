

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dormbr = require( './../lib' );

test( 'dormbr: main export is a function', function t() {
	assert.strictEqual( typeof dormbr, 'function' );
});

test( 'dormbr: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dormbr.ndarray, 'function' );
});

// TODO: Add implementation tests
