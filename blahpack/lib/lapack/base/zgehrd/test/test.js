

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zgehrd = require( './../lib' );

test( 'zgehrd: main export is a function', function t() {
	assert.strictEqual( typeof zgehrd, 'function' );
});

test( 'zgehrd: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgehrd.ndarray, 'function' );
});

// TODO: Add implementation tests
