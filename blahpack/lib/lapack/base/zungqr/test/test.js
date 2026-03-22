

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zungqr = require( './../lib' );

test( 'zungqr: main export is a function', function t() {
	assert.strictEqual( typeof zungqr, 'function' );
});

test( 'zungqr: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zungqr.ndarray, 'function' );
});

// TODO: Add implementation tests
