

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zungql = require( './../lib' );

test( 'zungql: main export is a function', function t() {
	assert.strictEqual( typeof zungql, 'function' );
});

test( 'zungql: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zungql.ndarray, 'function' );
});

// TODO: Add implementation tests
