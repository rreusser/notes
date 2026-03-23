

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dgehrd = require( './../lib' );

test( 'dgehrd: main export is a function', function t() {
	assert.strictEqual( typeof dgehrd, 'function' );
});

test( 'dgehrd: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dgehrd.ndarray, 'function' );
});

// TODO: Add implementation tests
