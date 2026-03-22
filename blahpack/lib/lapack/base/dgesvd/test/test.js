

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dgesvd = require( './../lib' );

test( 'dgesvd: main export is a function', function t() {
	assert.strictEqual( typeof dgesvd, 'function' );
});

test( 'dgesvd: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dgesvd.ndarray, 'function' );
});

// TODO: Add implementation tests
