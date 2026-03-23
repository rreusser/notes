

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var zgehd2 = require( './../lib' );

test( 'zgehd2: main export is a function', function t() {
	assert.strictEqual( typeof zgehd2, 'function' );
});

test( 'zgehd2: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof zgehd2.ndarray, 'function' );
});

// TODO: Add implementation tests
