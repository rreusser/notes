

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dpbtrs = require( './../lib' );

test( 'dpbtrs: main export is a function', function t() {
	assert.strictEqual( typeof dpbtrs, 'function' );
});

test( 'dpbtrs: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dpbtrs.ndarray, 'function' );
});

// TODO: Add implementation tests
