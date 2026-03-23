

'use strict';

var test = require( 'node:test' );
var assert = require( 'node:assert/strict' );
var ztbsv = require( './../lib' );

test( 'ztbsv: main export is a function', function t() {
	assert.strictEqual( typeof ztbsv, 'function' );
});

test( 'ztbsv: attached to the main export is an `ndarray` method', function t() {
	assert.strictEqual( typeof ztbsv.ndarray, 'function' );
});

// TODO: Add implementation tests
