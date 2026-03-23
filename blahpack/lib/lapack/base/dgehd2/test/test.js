

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dgehd2 = require( './../lib' );

test( 'dgehd2: main export is a function', function t() {
	assert.strictEqual( typeof dgehd2, 'function' );
});

test( 'dgehd2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dgehd2.ndarray, 'function' );
});

// TODO: Add implementation tests
