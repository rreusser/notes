

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zhseqr = require( './../lib' );

test( 'zhseqr: main export is a function', function t() {
	assert.strictEqual( typeof zhseqr, 'function' );
});

test( 'zhseqr: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zhseqr.ndarray, 'function' );
});

// TODO: Add implementation tests
