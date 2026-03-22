

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dgesv = require( './../lib' );

test( 'dgesv: main export is a function', function t() {
	assert.strictEqual( typeof dgesv, 'function' );
});

test( 'dgesv: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dgesv.ndarray, 'function' );
});

// TODO: Add implementation tests
