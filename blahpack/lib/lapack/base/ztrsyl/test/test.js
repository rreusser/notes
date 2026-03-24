

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var ztrsyl = require( './../lib' );

test( 'ztrsyl: main export is a function', function t() {
	assert.strictEqual( typeof ztrsyl, 'function' );
});

test( 'ztrsyl: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof ztrsyl.ndarray, 'function' );
});

// TODO: Add implementation tests
