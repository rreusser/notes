

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zung2l = require( './../lib' );

test( 'zung2l: main export is a function', function t() {
	assert.strictEqual( typeof zung2l, 'function' );
});

test( 'zung2l: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zung2l.ndarray, 'function' );
});

// TODO: Add implementation tests
