

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dtrexc = require( './../lib' );

test( 'dtrexc: main export is a function', function t() {
	assert.strictEqual( typeof dtrexc, 'function' );
});

test( 'dtrexc: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dtrexc.ndarray, 'function' );
});

// TODO: Add implementation tests
