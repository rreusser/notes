

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dtrevc3 = require( './../lib' );

test( 'dtrevc3: main export is a function', function t() {
	assert.strictEqual( typeof dtrevc3, 'function' );
});

test( 'dtrevc3: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dtrevc3.ndarray, 'function' );
});

// TODO: Add implementation tests
