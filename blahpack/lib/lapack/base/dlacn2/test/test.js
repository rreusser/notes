

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var dlacn2 = require( './../lib' );

test( 'dlacn2: main export is a function', function t() {
	assert.strictEqual( typeof dlacn2, 'function' );
});

test( 'dlacn2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof dlacn2.ndarray, 'function' );
});

// TODO: Add implementation tests
