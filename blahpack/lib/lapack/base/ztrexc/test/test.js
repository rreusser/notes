

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var ztrexc = require( './../lib' );

test( 'ztrexc: main export is a function', function t() {
	assert.strictEqual( typeof ztrexc, 'function' );
});

test( 'ztrexc: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof ztrexc.ndarray, 'function' );
});

// TODO: Add implementation tests
