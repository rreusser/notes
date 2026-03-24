

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var ztrevc3 = require( './../lib' );

test( 'ztrevc3: main export is a function', function t() {
	assert.strictEqual( typeof ztrevc3, 'function' );
});

test( 'ztrevc3: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof ztrevc3.ndarray, 'function' );
});

// TODO: Add implementation tests
